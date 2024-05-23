#pragma once

#include <QObject>
#include <QVariant>

extern "C" { QObject* ini(); }

class QT : public QObject {
  Q_OBJECT

public:
  // sensors
  Q_INVOKABLE QVariant iniSensors();
  Q_INVOKABLE QVariant heartRate();
  Q_INVOKABLE QVariant heartRateAccuracy();
};

