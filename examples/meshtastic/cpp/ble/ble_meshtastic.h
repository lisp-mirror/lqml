#pragma once

#include "ble.h"

#define UID QBluetoothUuid
#define STR QStringLiteral

#ifdef Q_OS_ANDROID
  class QtAndroidService;
#endif

class BLE_ME : public BLE {
  Q_OBJECT

public:
#ifdef Q_OS_ANDROID
  BLE_ME(QtAndroidService*);
#else
  BLE_ME();
#endif

  static const UID uuid_service;
  static const UID uuid_fromRadio;
  static const UID uuid_fromNum;
  static const UID uuid_toRadio;

  QLowEnergyCharacteristic fromRadio;
  QLowEnergyCharacteristic fromNum;
  QLowEnergyCharacteristic toRadio;

#ifdef Q_OS_ANDROID
  QtAndroidService* emitter = nullptr;
#else
  BLE_ME* emitter = nullptr;
#endif
  QString filter = "meshtastic";
  QLowEnergyDescriptor notifications;

  bool deviceFilter(const QBluetoothDeviceInfo&) override;
  
  void searchCharacteristics();

private Q_SLOTS:
  void ini();
  void serviceStateChanged(QLowEnergyService::ServiceState);
  void characteristicChanged(const QLowEnergyCharacteristic&, const QByteArray&);
  void characteristicRead(const QLowEnergyCharacteristic&, const QByteArray&);
  void characteristicWritten(const QLowEnergyCharacteristic&, const QByteArray&);
  void serviceError(QLowEnergyService::ServiceError);
  void disconnecting();

  /*** <INTERFACE> **********************************************************/

public Q_SLOTS:
  void setDeviceFilter(const QString& s) { filter = s; }
  void read();
  void write(const QByteArray&);

Q_SIGNALS:
  void setReady(bool, const QString& = QString(), const QStringList& = QStringList());
  void receivedFromRadio(const QByteArray&, const QString& = QString());
  void receivingDone();

  /*** </INTERFACE> *********************************************************/
};
