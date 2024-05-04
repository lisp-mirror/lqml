#pragma once

#include <QObject>
#include <QVariant>

class BLE_ME;
class WiFi_ME;

#ifndef NO_USB
  class USB_ME;
#endif

#ifdef Q_OS_ANDROID
  class QtAndroidService;
#endif

class Connection : public QObject {
  Q_OBJECT

public:
#ifdef Q_OS_ANDROID
  Connection(QtAndroidService*);
#else
  Connection();
#endif

  enum Type {
    BLE, USB, WiFi
  };

  Type type = BLE;
  BLE_ME* ble = nullptr;
  WiFi_ME* wifi = nullptr;
#ifndef NO_USB
  USB_ME* usb = nullptr;
#endif
  bool backgroundMode = false;

  void setConnectionType(const QVariant&);
  void startDeviceDiscovery(const QVariant&);
  void stopDeviceDiscovery();
  void disconnect();
  void setDeviceFilter(const QVariant&);
  void read2();
  void write2(const QVariant&);
  void setBackgroundMode(bool);

  // background mode
  void saveBytes(const QByteArray&);
  void sendSavedBytes();

Q_SIGNALS:
  void deviceDiscovered(const QVariant&);
  void bleError();
  void setReady(const QVariant&);
  void receivedFromRadio(const QVariant&);
  void receivingDone();
  void sendSavedPackets(const QVariant&);
};

