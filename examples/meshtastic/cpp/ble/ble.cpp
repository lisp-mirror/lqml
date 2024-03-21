#include "ble.h"
#include <QBluetoothAddress>
#include <QBluetoothDeviceDiscoveryAgent>
#include <QBluetoothDeviceInfo>
#include <QBluetoothServiceDiscoveryAgent>
#include <QList>
#include <QMetaEnum>
#include <QDebug>

BLE::BLE(const QBluetoothUuid& uuid) : mainServiceUuid(uuid) {
  discoveryAgent = new QBluetoothDeviceDiscoveryAgent();
  discoveryAgent->setLowEnergyDiscoveryTimeout(2000); // assumes android 'ScanSettings.SCAN_MODE_LOW_LATENCY' (see '../hacks/')

  connect(discoveryAgent, &QBluetoothDeviceDiscoveryAgent::deviceDiscovered,
          this, &BLE::addDevice);
#if QT_VERSION < 0x060000
  connect(discoveryAgent, QOverload<QBluetoothDeviceDiscoveryAgent::Error>::of(&QBluetoothDeviceDiscoveryAgent::error),
          this, &BLE::deviceScanError);
#else
  connect(discoveryAgent, &QBluetoothDeviceDiscoveryAgent::errorOccurred, this, &BLE::deviceScanError);
#endif
  connect(discoveryAgent, &QBluetoothDeviceDiscoveryAgent::finished, this, &BLE::deviceScanFinished);
}

void BLE::startDeviceDiscovery(const QString& name) {
  if (!name.isNull()) {
    initialDeviceName = name;
  }
  connected = false;
  scanned = false;
  currentDevice = QBluetoothDeviceInfo();
  devices.clear();

  qDebug() << "scanning for devices...";
  discoveryAgent->start(QBluetoothDeviceDiscoveryAgent::LowEnergyMethod);
}

void BLE::addDevice(const QBluetoothDeviceInfo& device) {
  if (deviceFilter(device)) {
    QString name(device.name());
    qDebug() << "device added:" << name;
    Q_EMIT deviceDiscovered(name);
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
    startDeviceDiscovery();
  } else {
    qDebug() << "device scan done";
    scanServices();
  }
}

void BLE::scanServices() {
  if (devices.isEmpty()) {
    return;
  }
  if (!currentDevice.isValid()) {
    if (!initialDeviceName.isEmpty()) {
      for (auto device : qAsConst(devices)) {
        if (device.name().contains(initialDeviceName, Qt::CaseInsensitive)) {
          currentDevice = device;
          break;
        }
      }
    }
  }
  if (!currentDevice.isValid()) {
    // fallback
    currentDevice = devices.at(0);
  }
  services.clear();
  qDebug() << "connecting to device...";
  if (controller) {
    Q_EMIT deviceDisconnecting();
    controller->disconnectFromDevice();
    delete controller; controller = nullptr;
  }

  if (!controller) {
    controller = QLowEnergyController::createCentral(currentDevice);
    connect(controller, &QLowEnergyController::connected,
            this, &BLE::deviceConnected);
#if QT_VERSION < 0x060000
    connect(controller, QOverload<QLowEnergyController::Error>::of(&QLowEnergyController::error),
            this, &BLE::errorReceived);
#else
    connect(controller, &QLowEnergyController::errorOccurred,
            this, &BLE::errorReceived);
    connect(controller, &QLowEnergyController::mtuChanged,
            [](int mtu) {
              qDebug() << "MTU changed to:" << mtu;
            });
#endif
    connect(controller, &QLowEnergyController::disconnected,
            this, &BLE::deviceDisconnected);
    connect(controller, &QLowEnergyController::serviceDiscovered,
            this, &BLE::addLowEnergyService);
    connect(controller, &QLowEnergyController::discoveryFinished,
            this, &BLE::serviceScanDone);
  }

  controller->connectToDevice();
}

void BLE::setCurrentDevice(const QBluetoothDeviceInfo& device) {
  if (device != currentDevice) {
    currentDevice = device;
    scanned = false;
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
#if QT_VERSION >= 0x060000
  qDebug() << "MTU:" << controller->mtu();
#endif
}

void BLE::connectToService(const QString& uuid) {
  QLowEnergyService* service = nullptr;
  for (auto s : qAsConst(services)) {
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
    scanServices();
  }
}

void BLE::errorReceived(QLowEnergyController::Error) {
  qDebug() << "BLE error: " << controller->errorString();
  Q_EMIT bleError();
}

void BLE::disconnectFromDevice() {
  if (controller->state() != QLowEnergyController::UnconnectedState) {
    controller->disconnectFromDevice();
  } else {
    deviceDisconnected();
  }
}

void BLE::deviceDisconnected() {
  qDebug() << "disconnected from device";
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
}
