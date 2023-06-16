#include "qt.h"
#include "ble_meshtastic.h"
#include <QtDebug>

QT_BEGIN_NAMESPACE

QObject* ini() {
  static QObject* qt = nullptr;
  if (qt == nullptr) {
    qt = new QT;
  }
  return qt;
}

QT::QT() : QObject() {
  ble = new BLE_ME;
}

QVariant QT::setDevice(const QVariant& vName) {
  auto name = vName.toString();
  for (auto device : qAsConst(ble->devices)) {
    if (device.name().contains(name, Qt::CaseInsensitive)) {
      ble->setCurrentDevice(device);
      return vName;
    }
  }
  return QVariant();
}

QVariant QT::startDeviceDiscovery(const QVariant& vName) {
  auto name = vName.toString();
  if (!name.isNull()) {
    ble->initialDeviceName = name;
  }
  ble->startDeviceDiscovery();
  return vName;
}

QVariant QT::read2() {
  ble->read();
  return QVariant();
}

QVariant QT::write2(const QVariant& bytes) {
  ble->write(bytes.toByteArray());
  return QVariant();
}

QT_END_NAMESPACE
