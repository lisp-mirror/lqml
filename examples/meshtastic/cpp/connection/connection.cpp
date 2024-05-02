#include "connection.h"
#include "ble/ble_me.h"
#include "wifi/wifi_me.h"
#include <QStandardPaths>
#include <QFile>
#include <QDataStream>

#ifdef Q_OS_ANDROID
  #include "../android_service/qtandroidservice_ro.h"
  #if (QT_VERSION < 0x060000)
    #include <QAndroidService>
  #endif
#else
  #include "usb/usb_me.h"
#endif

#ifdef Q_OS_ANDROID
Connection::Connection(QtAndroidService* service) {
  service->con = this;
  // forward signal
  connect(this, &Connection::sendSavedPackets, service, &QtAndroidService::sendSavedPackets);
  ble = new BLE_ME(service, this);
  wifi = new WiFi_ME(service, this);
}
#else
Connection::Connection() {
  ble = new BLE_ME(this);
  usb = new USB_ME(this);
  wifi = new WiFi_ME(this);
}
#endif

void Connection::setConnectionType(const QVariant& var) {
  type = Type(BLE + (QList<QByteArray>() << "BLE" << "USB" << "WIFI")
                    .indexOf(var.toByteArray()));
}

void Connection::startDeviceDiscovery(const QVariant& var) {
  switch (type) {
    case BLE:
#ifndef Q_OS_ANDROID
      usb->disconnect();
#endif
      wifi->disconnect();
      ble->startDeviceDiscovery(var.toString());
      break;
    case USB:
      ble->disconnect();
      wifi->disconnect();
#ifndef Q_OS_ANDROID
      usb->connectToRadio();
#endif
      break;
    case WiFi:
      ble->disconnect();
#ifndef Q_OS_ANDROID
      usb->disconnect();
#endif
      wifi->connectToRadio(var.toString());
      break;
  }
}

void Connection::stopDeviceDiscovery() {
  ble->stopDeviceDiscovery();
}

void Connection::disconnect() {
  switch (type) {
    case BLE: ble->disconnect(); break;
#ifndef Q_OS_ANDROID
    case USB: usb->disconnect(); break;
#endif
    case WiFi: wifi->disconnect(); break;
  }
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
#ifndef Q_OS_ANDROID
      usb->write2(bytes);
#endif
      break;
    case WiFi:
      wifi->write2(bytes);
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
