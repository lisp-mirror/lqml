#include "qt.h"

#if (QT_VERSION < 0x060000)
  #include <QtAndroid>
  #include <QAndroidJniEnvironment>
#else
  #include <QtCore/private/qandroidextras_p.h>
#endif

QT_BEGIN_NAMESPACE

QVariant QT::keepScreenOn(const QVariant& on) {
#if (QT_VERSION < 0x060000)
  QtAndroid::runOnAndroidThread([&] {
    QAndroidJniObject activity = QtAndroid::androidActivity();
    if (activity.isValid()) {
      QAndroidJniObject window = activity.callObjectMethod("getWindow", "()Landroid/view/Window;");
      if (window.isValid()) {
        const int FLAG_KEEP_SCREEN_ON = 128;
        const char* method = on.toBool() ? "addFlags" : "clearFlags";
        window.callMethod<void>(method, "(I)V", FLAG_KEEP_SCREEN_ON);
      }
    }
    QAndroidJniEnvironment env;
    if (env->ExceptionCheck()) {
      env->ExceptionClear();
    }
  });
#else
  QNativeInterface::QAndroidApplication::runOnAndroidMainThread([&] {
    QJniObject activity = QtAndroidPrivate::activity();
    if (activity.isValid()) {
      QJniObject window = activity.callObjectMethod("getWindow", "()Landroid/view/Window;");
      if (window.isValid()) {
        const int FLAG_KEEP_SCREEN_ON = 128;
        const char* method = on.toBool() ? "addFlags" : "clearFlags";
        window.callMethod<void>(method, "(I)V", FLAG_KEEP_SCREEN_ON);
      }
    }
    QJniEnvironment env;
    if (env->ExceptionCheck()) {
      env->ExceptionClear();
    }
  });
#endif
  return on;
}

QT_END_NAMESPACE
