#include "connection.h"
#include "ble/ble_meshtastic.h"
#include "usb/usb_meshtastic.h"
#include <QStandardPaths>
#include <QFile>
#include <QDataStream>

#ifdef Q_OS_ANDROID
  #include "../android_service/qtandroidservice_ro.h"
  #if (QT_VERSION < 0x060000)
    #include <QAndroidService>
  #endif
#endif

#ifdef Q_OS_ANDROID
Connection::Connection(QtAndroidService* service) {
  // forward signal
  connect(this, &Connection::sendSavedPackets, service, &QtAndroidService::sendSavedPackets);
  ble = new BLE_ME(service, service->con);
  usb = new USB_ME(service, service->con);
}
#else
Connection::Connection() {
  ble = new BLE_ME(this);
  usb = new USB_ME(this);
}
#endif

void Connection::setConnectionType(const QVariant& vType) {
  QByteArray t = vType.toByteArray();
  if (t == "USB") {
    type = USB;
  } else {
    type = BLE;
  }
}

void Connection::startDeviceDiscovery(const QVariant& vName) {
  switch (type) {
    case BLE:
      usb->disconnect();
      ble->startDeviceDiscovery(vName.toString());
      break;
    case USB:
      ble->disconnect();
      usb->connectToRadio();
      break;
  }
}

void Connection::stopDeviceDiscovery() {
  ble->stopDeviceDiscovery();
}

void Connection::setDeviceFilter(const QVariant& vName) {
  ble->setDeviceFilter(vName.toString());
}

void Connection::read2() {
  ble->read();
}

void Connection::write2(const QVariant& vBytes) {
  QByteArray bytes = vBytes.toByteArray();
  switch (type) {
    case BLE:
      ble->write(bytes);
      break;
    case USB:
      usb->write2(bytes);
      break;
  }
}

// background mode

void Connection::setBackgroundMode(bool background) {
#if (defined Q_OS_ANDROID) || (defined Q_OS_IOS)
  backgroundMode = background;
  qDebug() << "background mode:" << backgroundMode;
  if (!backgroundMode) {
    sendSavedBytes();
  }
#endif
}

static QString packetsFile() {
  // choose already existing directory
  QStandardPaths::StandardLocation location = QStandardPaths::AppDataLocation;
#ifdef Q_OS_IOS
  location = QStandardPaths::DocumentsLocation;
#endif
  return QStandardPaths::writableLocation(location) + QStringLiteral("/meshtastic-packets.bin");
}

void Connection::saveBytes(const QByteArray& packet) {
  QFile file(packetsFile());
  if (file.open(QIODevice::WriteOnly | QIODevice::Append)) {
    QDataStream ds(&file);
    ds << packet;
    file.close();
  }
}

void Connection::sendSavedBytes() {
  QVariantList packets;
  QString fileName(packetsFile());
  QFile file(fileName);
  if (file.open(QIODevice::ReadOnly)) {
    QDataStream ds(&file);
    while (!ds.atEnd()) {
      QByteArray packet;
      ds >> packet;
      packets.append(packet);
    }
    file.close();
    if (!packets.isEmpty()) {
      Q_EMIT sendSavedPackets(QVariant(packets));
      QFile::remove(fileName);
    }
  }
}
