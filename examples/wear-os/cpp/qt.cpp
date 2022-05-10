#undef SLOT

#include "qt.h"
#include <lqml.h>
#include <ecl/ecl.h>
#include <QtAndroid> 
#include <QAndroidJniEnvironment>

static int getIntField(const char* name) {
  QAndroidJniObject activity = QtAndroid::androidActivity();
  return static_cast<int>(activity.getField<jint>(name));
}

static void clearEventualExceptions() {
  QAndroidJniEnvironment env;
  if (env->ExceptionCheck()) {
    env->ExceptionClear();
  }
}

QObject* ini() {
  static QObject* qt = nullptr;
  if (!qt) {
    qt = new QT;
  }
  return qt;
}

// sensors

QVariant QT::iniSensors() {
  QtAndroid::runOnAndroidThread([] {
    QAndroidJniObject activity = QtAndroid::androidActivity();
    activity.callMethod<void>("iniSensors", "()V");
    clearEventualExceptions();
  });
  return QVariant();
}

QVariant QT::heartRate() {
  return getIntField("_heart_rate_");
}

QVariant QT::heartRateAccuracy() {
  return getIntField("_heart_rate_accuracy_");
}
