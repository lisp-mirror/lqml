#pragma once

#include <QtCore>

#ifdef Q_CC_MSVC
#define LIB_EXPORT __declspec(dllexport)
#else
#define LIB_EXPORT
#endif

class BLE_ME;

QT_BEGIN_NAMESPACE

extern "C" { LIB_EXPORT QObject* ini(); }

class QT : public QObject {
  Q_OBJECT

public:
  // BLE_ME
  Q_INVOKABLE QVariant startDeviceDiscovery();
  Q_INVOKABLE QVariant read2();
  Q_INVOKABLE QVariant write2(const QVariant&);

  QT();

  BLE_ME* ble;
};

QT_END_NAMESPACE
