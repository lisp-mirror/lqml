#include "qt.h"
#include <ecl_fun.h>

#if (QT_VERSION < 0x060000)
  #define JniObject QAndroidJniObject
  #include <QtAndroid>
  #include <QAndroidJniEnvironment>
#else
  #define JniObject QJniObject
  #include <QtCore/private/qandroidextras_p.h>
#endif

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

static JniObject androidActivity() {
#if (QT_VERSION < 0x060000)
  return QtAndroid::androidActivity();
#else
  return QtAndroidPrivate::activity();
#endif
}

// keep screen on

QVariant QT::keepScreenOn(const QVariant& on) {
#if (QT_VERSION < 0x060000)
  QtAndroid::runOnAndroidThread([&] {
    JniObject activity = androidActivity();
    if (activity.isValid()) {
      QAndroidJniObject window = activity.callObjectMethod("getWindow", "()Landroid/view/Window;");
      if (window.isValid()) {
        const int FLAG_KEEP_SCREEN_ON = 128;
        const char* method = on.toBool() ? "addFlags" : "clearFlags";
        window.callMethod<void>(
          method,
          "(I)V", FLAG_KEEP_SCREEN_ON);
      }
    }
    clearEventualExceptions();
  });
#else
  QNativeInterface::QAndroidApplication::runOnAndroidMainThread([&] {
    JniObject activity = androidActivity();
    if (activity.isValid()) {
      QJniObject window = activity.callObjectMethod("getWindow", "()Landroid/view/Window;");
      if (window.isValid()) {
        const int FLAG_KEEP_SCREEN_ON = 128;
        const char* method = on.toBool() ? "addFlags" : "clearFlags";
        window.callMethod<void>(
          method,
          "(I)V", FLAG_KEEP_SCREEN_ON);
      }
    }
    clearEventualExceptions();
  });
#endif
  return on;
}

// GPS

static qlonglong getLongField(const char* name) {
  JniObject activity = androidActivity();
  return static_cast<qlonglong>(activity.getField<jlong>(name));
}

static double getDoubleField(const char* name) {
  JniObject activity = androidActivity();
  return static_cast<double>(activity.getField<jdouble>(name));
}

QVariant QT::iniPositioning() {
#if (QT_VERSION < 0x060000)
  QtAndroid::runOnAndroidThread([] {
    androidActivity().callMethod<void>(
      "iniLocation",
      "()V");
    clearEventualExceptions();
  });
#else
  QNativeInterface::QAndroidApplication::runOnAndroidMainThread([&] {
    androidActivity().callMethod<void>(
      "iniLocation",
      "()V");
    clearEventualExceptions();
  });
#endif
  return QVariant();
}

QVariant QT::lastPosition() {
  QVariantList pos;
  pos << getDoubleField("position_lat")
      << getDoubleField("position_lon")
      << getDoubleField("position_alt")
      << getLongField("position_time");
  return pos;
}

// USB

static QT* instance() {
  return QT::_this;
}

static void javaUsbDeviceAttached(JNIEnv*, jobject) { // see Java
  // run on Qt thread
  QMetaObject::invokeMethod(
    instance(),
    "usbDeviceAttached");
}

void QT::usbDeviceAttached() {
  ecl_fun("radios:device-discovered", QStringLiteral("USB"));
}

void QT::iniJni() {
  JNINativeMethod methods[] {
    { "qtUsbDeviceAttached", "()V", reinterpret_cast<void*>(javaUsbDeviceAttached) }
  };
#if (QT_VERSION < 0x060000)
  QAndroidJniEnvironment env;
#else
  QJniEnvironment env;
#endif
  jclass jcl = env->GetObjectClass(androidActivity().object<jobject>());
  env->RegisterNatives(jcl, methods, sizeof(methods) / sizeof(methods[0]));
  clearEventualExceptions();
}

