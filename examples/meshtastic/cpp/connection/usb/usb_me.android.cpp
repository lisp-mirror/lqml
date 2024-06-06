#include "usb_me.android.h"
#include "../connection.h"
#include "../../android_service/qtandroidservice_ro.h"
#include <QTimer>
#include <QtDebug>

#if (QT_VERSION < 0x060000)
  #define JniObject QAndroidJniObject
  #include <QtAndroid>
  #include <QAndroidJniEnvironment>
#else
  #define JniObject QJniObject
  #include <QtCore/private/qandroidextras_p.h>
#endif

USB_ME* USB_ME::_this = nullptr;

static USB_ME* instance() {
  return USB_ME::_this;
}

static void clearEventualExceptions() {
#if (QT_VERSION < 0x060000)
  QAndroidJniEnvironment env;
#else
  QJniEnvironment env;
#endif
  if (env->ExceptionCheck()) {
    env->ExceptionClear();
  }
}

static JniObject androidService() {
#if (QT_VERSION < 0x060000)
  return QtAndroid::androidService();
#else
  return QtAndroidPrivate::service();
#endif
}

static void javaSetReady(JNIEnv*, jobject, jboolean jb) { // see Java
  bool ready = (bool)jb;
  instance()->ready = ready;
  instance()->emitter->setReady(QVariant(QVariantList() << ready));
}

static void javaOnNewData(JNIEnv* env, jobject, jbyteArray jba) { // see Java
  jbyte* jb = env->GetByteArrayElements(jba, NULL);
  jsize js = env->GetArrayLength(jba);
  QByteArray ba((char*)jb, js);
  // run on Qt thread
  QMetaObject::invokeMethod(
    instance(),
    "onNewData",
    Q_ARG(QByteArray, ba));
  env->ReleaseByteArrayElements(jba, jb, JNI_ABORT);
  clearEventualExceptions();
}

USB_ME::USB_ME(QtAndroidService* service, Connection* _con) : emitter(service), con(_con) {
  _this = this;

  // ini JNI
  JNINativeMethod methods[] {
    { "qtSetReady",  "(Z)V",  reinterpret_cast<void*>(javaSetReady) },
    { "qtOnNewData", "([B)V", reinterpret_cast<void*>(javaOnNewData) }
  };
  JniObject service2 = androidService();
#if (QT_VERSION < 0x060000)
  QAndroidJniEnvironment env;
#else
  QJniEnvironment env;
#endif
  jclass jcl = env->GetObjectClass(service2.object<jobject>());
  env->RegisterNatives(jcl, methods, sizeof(methods) / sizeof(methods[0]));
  clearEventualExceptions();

  // ini
  timer.setSingleShot(true);
  connect(&timer, &QTimer::timeout, this, &USB_ME::done);
}

void USB_ME::connectToRadio() {
  JniObject service = androidService();
  service.callMethod<void>(
    "iniUsb",
    "()V");
  clearEventualExceptions();
}

void USB_ME::disconnect() {
  // unused
}

void USB_ME::write2(const QByteArray& ba) {
  if (ready) {
#if (QT_VERSION < 0x060000)
    QAndroidJniEnvironment env;
#else
    QJniEnvironment env;
#endif
    jbyteArray jba = env->NewByteArray(ba.size());
    env->SetByteArrayRegion(jba, 0, ba.size(), (jbyte*)ba.data());
    JniObject service = androidService();
    service.callMethod<void>(
      "writeUsb",
      "([B)V", jba);
    clearEventualExceptions();
    emitter->sendingDone();
  } else {
    qDebug() << "USB not ready: write()";
  }
}

void USB_ME::read2() {
  // unused
}

void USB_ME::onNewData(const QByteArray& data) { // see JNI
  packets << data;
  timer.start(1000); // assume receiving done after pause of 1 sec
}

void USB_ME::received(const QByteArray& data) {
  if (!data.isEmpty()) {
    if (con->backgroundMode) {
      con->saveBytes(data);
    } else {
      emitter->receivedFromRadio(QVariant(QVariantList() << data));
    }
  }
}

void USB_ME::done() {
  if (!con->backgroundMode) {
    static bool startup = true;
    if (startup) {
      con->sendSavedBytes(); // for eventual, saved but not sent packets
    } else {
      startup = false;
    }
  }

  const QByteArray HEADER = QByteArray::fromHex("94c3");
  const int LEN = 4;
  QByteArray data(packets.join());
  packets.clear();
  int start = 0;
  while ((start = data.indexOf(HEADER, start)) != -1) {
    int i_len = start + 2;
    int len = (data.at(i_len) << 8) + data.at(i_len + 1);
    received(data.mid(start + LEN, len));
    start += LEN + len;
  }

  emitter->receivingDone();
}

