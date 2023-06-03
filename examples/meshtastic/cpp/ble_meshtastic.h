#pragma once

#include "ble.h"

#define UUID QBluetoothUuid
#define STR  QStringLiteral

class BLE_ME : public BLE {
  Q_OBJECT

public:
  BLE_ME();

  static const UUID uuid_service;
  static const UUID uuid_fromRadio;
  static const UUID uuid_fromNum;
  static const UUID uuid_toRadio;

  QLowEnergyCharacteristic fromRadio;
  QLowEnergyCharacteristic fromNum;
  QLowEnergyCharacteristic toRadio;

  QLowEnergyDescriptor notifications;

  bool deviceFilter(const QBluetoothDeviceInfo&) override;
  
  void write(const QByteArray&);
  void searchCharacteristics();

public Q_SLOTS:
  void ini();
  void read();
  void serviceStateChanged(QLowEnergyService::ServiceState);
  void characteristicChanged(const QLowEnergyCharacteristic&, const QByteArray&);
  void characteristicRead(const QLowEnergyCharacteristic&, const QByteArray&);
  void characteristicWritten(const QLowEnergyCharacteristic&, const QByteArray&);
  void serviceError(QLowEnergyService::ServiceError);
  void disconnecting();
};
