#include "connection.h"
#include "wifi/wifi_me.h"
#include <QStandardPaths>
#include <QFile>
#include <QDataStream>

#ifndef NO_BLE
  #include "ble/ble_me.h"
#endif
#ifndef NO_USB
  #ifdef Q_OS_ANDROID
    #include "usb/usb_me.android.h"
  #else
    #include "usb/usb_me.h"
  #endif
#endif

#ifdef Q_OS_ANDROID
  #include "../android_service/qtandroidservice_ro.h"
  #if (QT_VERSION < 0x060000)
    #include <QAndroidService>
  #endif
#endif

#ifdef Q_OS_ANDROID
Connection::Connection(QtAndroidService* service) {
  service->con = this;
  emitter = service;
  ble = new BLE_ME(service, this);
  usb = new USB_ME(service, this);
  wifi = new WiFi_ME(service, this);
}
#else
Connection::Connection() {
  emitter = this;
#ifndef NO_BLE
  ble = new BLE_ME(this);
#endif
#ifndef NO_USB
  usb = new USB_ME(this);
#endif
  wifi = new WiFi_ME(this);
}
#endif

void Connection::setConnectionType(const QVariant& var) {
  type = Type(QByteArrayList({"BLE", "USB", "WIFI"})
              .indexOf(var.toByteArray()));
}

void Connection::startDeviceDiscovery(const QVariant& var) {
  switch (type) {
    case BLE:
#ifndef NO_BLE
  #ifndef NO_USB
      usb->disconnect();
  #endif
      wifi->disconnect();
      ble->startDeviceDiscovery(var.toString());
#endif
      break;
    case USB:
#ifndef NO_USB
  #ifndef NO_BLE
      ble->disconnect();
  #endif
      wifi->disconnect();
      usb->connectToRadio();
#endif
      break;
    case WiFi:
#ifndef NO_BLE
      ble->disconnect();
#endif
#ifndef NO_USB
      usb->disconnect();
#endif
      wifi->connectToRadio(var.toString());
      break;
  }
}

void Connection::stopDeviceDiscovery() {
#ifndef NO_BLE
  ble->stopDeviceDiscovery();
#endif
}

void Connection::disconnect() {
  switch (type) {
    case BLE:
#ifndef NO_BLE
      ble->disconnect();
#endif
      break;
    case USB:
#ifndef NO_USB
      usb->disconnect();
#endif
      break;
    case WiFi:
      wifi->disconnect();
      break;
  }
}

void Connection::setDeviceFilter(const QVariant& vName) {
#ifndef NO_BLE
  ble->setDeviceFilter(vName.toString());
#endif
}

void Connection::read2() {
#ifndef NO_BLE
  ble->read();
#endif
}

void Connection::write2(const QVariant& vBytes) {
  QByteArray bytes = vBytes.toByteArray();
  switch (type) {
    case BLE:
#ifndef NO_BLE
      ble->write(bytes);
#endif
      break;
    case USB:
#ifndef NO_USB
      usb->write2(bytes);
#endif
      break;
    case WiFi:
      wifi->write2(bytes);
      break;
  }
}

void Connection::received(const QByteArray& data) {
  if (!data.isEmpty()) {
    if (backgroundMode) {
      saveBytes(data);
    } else {
      emitter->receivedFromRadio(QVariant(QVariantList() << data));
    }
  }
}

void Connection::done(QByteArrayList& packets) {
  if (!backgroundMode) {
    static bool startup = true;
    if (startup) {
      sendSavedBytes(); // for eventual, saved but not sent packets
    } else {
      startup = false;
    }
  }

  const QByteArray HEADER = QByteArray::fromHex("94c3");
  const int LEN = 4;
  QByteArray data(packets.join());
  packets.clear();
  int start = 0;
  while ((start = data.indexOf(HEADER, start)) != -1) {
    int i_len = start + 2;
    int len = (data.at(i_len) << 8) + data.at(i_len + 1);
    received(data.mid(start + LEN, len));
    start += LEN + len;
  }

  emitter->receivingDone();
}

// background mode

void Connection::setBackgroundMode(bool background) {
#if (defined Q_OS_ANDROID) || (defined Q_OS_IOS)
  backgroundMode = background;
  qDebug() << "background mode:" << backgroundMode;
  if (!backgroundMode) {
    sendSavedBytes();
  }
#else
  Q_UNUSED(background)
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
      emitter->sendSavedPackets(QVariant(packets));
      QFile::remove(fileName);
    }
  }
}
