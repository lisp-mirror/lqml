#include "qt.h"
#include <QtAndroid> 
#include <QAndroidJniEnvironment>

QT_BEGIN_NAMESPACE

QVariant QT::keepScreenOn(const QVariant& on) {
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
  return on;
}

QT_END_NAMESPACE
