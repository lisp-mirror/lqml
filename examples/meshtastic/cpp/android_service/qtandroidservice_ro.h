#include "rep_qtandroidservice_source.h"
#include "../ble/ble_meshtastic.h"

class QtAndroidService : public QtAndroidServiceSource {
public:
  BLE_ME* ble = nullptr;

public slots:
  void startDeviceDiscovery(const QString& a1) override { ble->startDeviceDiscovery(a1); }
  void setDeviceFilter(const QString& a1)      override { ble->setDeviceFilter(a1); }
  void read()                                  override { ble->read(); }
  void write(const QByteArray& a1)             override { ble->write(a1); }
};
