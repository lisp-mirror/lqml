#pragma once

#include "ble.h"

#define UID QBluetoothUuid
#define STR QStringLiteral

#ifdef Q_OS_ANDROID
  class QtAndroidService;
#endif

class BLE_ME : public BLE {
  Q_OBJECT

  /*** <INTERFACE> ****************************************/

public:
#ifdef Q_OS_ANDROID
  BLE_ME(QtAndroidService*);
#else
  BLE_ME();
#endif

public Q_SLOTS:
  void setDeviceFilter(const QString& s) { filter = s; }
  void read();
  void write(const QByteArray&);
  void setBackgroundMode(bool);

Q_SIGNALS:
  void setReady(bool, const QString&, const QStringList&);
  void receivedFromRadio(const QByteArray&, const QString&);
  void receivingDone();
  void sendSavedPackets(const QVariant&);

  /*** </INTERFACE> ***************************************/

public:
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
  bool backgroundMode = false;
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

private:
  void saveBytes(const QByteArray&);
  void sendSavedBytes();
};
