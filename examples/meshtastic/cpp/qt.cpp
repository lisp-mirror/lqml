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

static QBluetoothDeviceInfo toDeviceInfo(const QVariantMap& map) {
  return QBluetoothDeviceInfo(QBluetoothAddress(map.value("address").toString()),
                              map.value("name").toString(),
                              0);
}

QT::QT() : QObject() {
  ble = new BLE_ME;
}

QVariant QT::setDevice(const QVariant& vMap) {
  auto map = vMap.value<QVariantMap>();
  ble->setCurrentDevice(toDeviceInfo(map));
  return vMap;
}

QVariant QT::startDeviceDiscovery(const QVariant& vMap) {
  auto map = vMap.value<QVariantMap>();
  if (!map.isEmpty()) {
    ble->currentDevice = toDeviceInfo(map);
  }
  ble->startDeviceDiscovery();
  return vMap;
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
