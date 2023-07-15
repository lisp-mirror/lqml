// inspired by Qt5 example 'lowenergyscanner'

#pragma once

#include <QObject>
#include <QVariant>
#include <QList>
#include <QBluetoothServiceDiscoveryAgent>
#include <QBluetoothDeviceDiscoveryAgent>
#include <QLowEnergyController>
#include <QBluetoothServiceInfo>

class BLE: public QObject {
  Q_OBJECT

public:
  BLE(const QBluetoothUuid& = QBluetoothUuid()); // pass 'mainServiceUuid'

  /*** <INTERFACE> **********************************************************/

  // main service and its UUID
  QLowEnergyService* mainService = nullptr;
  QBluetoothUuid mainServiceUuid; // see constructor

  // current device
  QBluetoothDeviceInfo currentDevice; // if not defined, first one discovered
  QList<QBluetoothDeviceInfo> devices;
  void setCurrentDevice(const QBluetoothDeviceInfo&);
  QString initialDeviceName; // optional

  // custom device filter (e.g. name filter)
  virtual bool deviceFilter(const QBluetoothDeviceInfo&) { return true; }

Q_SIGNALS:
  // notify
  void deviceDiscovered(const QString&);
  void mainServiceReady();
  void deviceDisconnecting();

public Q_SLOTS:
  void startDeviceDiscovery();

  /*** </INTERFACE> *********************************************************/

  void scanServices();
  void connectToService(const QString&);
  void disconnectFromDevice();

private Q_SLOTS:
  // QBluetoothDeviceDiscoveryAgent related
  void addDevice(const QBluetoothDeviceInfo&);
  void deviceScanFinished();
  void deviceScanError(QBluetoothDeviceDiscoveryAgent::Error);

  // QLowEnergyController realted
  void addLowEnergyService(const QBluetoothUuid&);
  void deviceConnected();
  void errorReceived(QLowEnergyController::Error);
  void serviceScanDone();
  void deviceDisconnected();

private:
  void retryScan();
  QBluetoothDeviceDiscoveryAgent* discoveryAgent;
  QList<QLowEnergyService*> services;
  QLowEnergyController* controller = nullptr;
  bool connected = false;
  bool scanned = false;
};

