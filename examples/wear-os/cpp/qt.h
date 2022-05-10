#pragma once

#include <QObject>
#include <QVariant>

QT_BEGIN_NAMESPACE

extern "C" { QObject* ini(); }

class QT : public QObject {
  Q_OBJECT

public:
  // sensors
  Q_INVOKABLE QVariant iniSensors();
  Q_INVOKABLE QVariant heartRate();
  Q_INVOKABLE QVariant heartRateAccuracy();
};

QT_END_NAMESPACE
