#include "ble.h"
#include <QBluetoothAddress>
#include <QBluetoothDeviceDiscoveryAgent>
#include <QBluetoothDeviceInfo>
#include <QBluetoothServiceDiscoveryAgent>
#include <QList>
#include <QMetaEnum>
#include <QTimer>
#include <QDebug>

BLE::BLE(const QBluetoothUuid& uuid) : mainServiceUuid(uuid) {
  discoveryAgent = new QBluetoothDeviceDiscoveryAgent();
  discoveryAgent->setLowEnergyDiscoveryTimeout(5000);

  connect(discoveryAgent, &QBluetoothDeviceDiscoveryAgent::deviceDiscovered,
          this, &BLE::addDevice);
  connect(discoveryAgent, QOverload<QBluetoothDeviceDiscoveryAgent::Error>::of(&QBluetoothDeviceDiscoveryAgent::error),
          this, &BLE::deviceScanError);
  connect(discoveryAgent, &QBluetoothDeviceDiscoveryAgent::finished, this, &BLE::deviceScanFinished);

  QTimer::singleShot(0, this, &BLE::startDeviceDiscovery);
}

void BLE::startDeviceDiscovery() {
  devices.clear();
  qDebug() << "scanning for devices...";
  discoveryAgent->start(QBluetoothDeviceDiscoveryAgent::LowEnergyMethod);
}

void BLE::addDevice(const QBluetoothDeviceInfo& device) {
  if (deviceFilter(device)) {
    qDebug() << "device added: " << device.name();
  }
}

void BLE::deviceScanFinished() {
  const QList<QBluetoothDeviceInfo> found = discoveryAgent->discoveredDevices();
  for (auto device : found) {
    if (deviceFilter(device)) {
      devices << device;
    }
  }
  if (devices.isEmpty()) {
    qDebug() << "no BLE devices found";
  } else {
    qDebug() << "device scan done";
  }
  QTimer::singleShot(0, this, &BLE::scanServices);
}

void BLE::scanServices() {
  if (devices.isEmpty()) {
    return;
  }
  if (!currentDevice.isValid()) {
    currentDevice = devices.at(0);
  }
  services.clear();
  qDebug() << "connecting to device...";
  if (controller && (previousAddress != currentDevice.address())) {
    Q_EMIT deviceDisconnecting();
    controller->disconnectFromDevice();
    delete controller;
    controller = nullptr;
  }

  if (!controller) {
    controller = QLowEnergyController::createCentral(currentDevice);
    connect(controller, &QLowEnergyController::connected,
            this, &BLE::deviceConnected);
    connect(controller, QOverload<QLowEnergyController::Error>::of(&QLowEnergyController::error),
            this, &BLE::errorReceived);
    connect(controller, &QLowEnergyController::disconnected,
            this, &BLE::deviceDisconnected);
    connect(controller, &QLowEnergyController::serviceDiscovered,
            this, &BLE::addLowEnergyService);
    connect(controller, &QLowEnergyController::discoveryFinished,
            this, &BLE::serviceScanDone);
  }

  controller->connectToDevice();
  previousAddress = currentDevice.address();
}

void BLE::setCurrentDevice(const QBluetoothDeviceInfo& device) {
  if (device != currentDevice) {
    currentDevice = device;
    scanned = false;
    retryScan();
  }
}

void BLE::addLowEnergyService(const QBluetoothUuid& serviceUuid) {
  QLowEnergyService* service = controller->createServiceObject(serviceUuid);
  if (!service) {
    qDebug() << "cannot create service for UUID";
    return;
  }
  services << service;

  if (serviceUuid == mainServiceUuid) {
    mainService = service;
  }
}

void BLE::serviceScanDone() {
  scanned = true;
  Q_EMIT mainServiceReady();
  qDebug() << "service scan done";
}

void BLE::connectToService(const QString& uuid) {
  QLowEnergyService* service = nullptr;
  for (auto s: qAsConst(services)) {
    if (s->serviceUuid().toString() == uuid) {
      service = s;
      break;
    }
  }

  if (!service) {
    return;
  }

  if (service->state() == QLowEnergyService::DiscoveryRequired) {
    service->discoverDetails();
    qDebug() << "discovering details...";
    return;
  }
}

void BLE::deviceConnected() {
  connected = true;
  controller->discoverServices();
}

void BLE::retryScan() {
  if (connected && !scanned) {
    QTimer::singleShot(0, this, &BLE::scanServices);
  }
}

void BLE::errorReceived(QLowEnergyController::Error) {
  qDebug() << "BLE error: " << controller->errorString();
  retryScan();
}

void BLE::disconnectFromDevice() {
  if (controller->state() != QLowEnergyController::UnconnectedState) {
    controller->disconnectFromDevice();
  } else {
    deviceDisconnected();
  }
}

void BLE::deviceDisconnected() {
  connected = false;
  qDebug() << "disconnect from device";
}

void BLE::deviceScanError(QBluetoothDeviceDiscoveryAgent::Error error) {
  if (error == QBluetoothDeviceDiscoveryAgent::PoweredOffError) {
    qDebug() << "the Bluetooth adaptor is powered off, power it on before doing discovery";
  } else if (error == QBluetoothDeviceDiscoveryAgent::InputOutputError) {
    qDebug() << "writing or reading from the device resulted in an error";
  } else {
    static QMetaEnum qme = discoveryAgent->metaObject()->enumerator(
           discoveryAgent->metaObject()->indexOfEnumerator("Error"));
    qDebug() << "error: " + QLatin1String(qme.valueToKey(error));
  }
  retryScan();
}

