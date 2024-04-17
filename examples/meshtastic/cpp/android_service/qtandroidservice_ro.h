#include "rep_qtandroidservice_source.h"
#include "../connection/connection.h"

class QtAndroidService : public QtAndroidServiceSource {
public:
  Connection* con = nullptr;

public slots:
  void setConnectionType(const QVariant& a1)    override { con->setConnectionType(a1); }
  void startDeviceDiscovery(const QVariant& a1) override { con->startDeviceDiscovery(a1); }
  void stopDeviceDiscovery()                    override { con->stopDeviceDiscovery(); }
  void setDeviceFilter(const QVariant& a1)      override { con->setDeviceFilter(a1); }
  void read2()                                  override { con->read2(); }
  void write2(const QVariant& a1)               override { con->write2(a1); }
  void setBackgroundMode(bool a1)               override { con->setBackgroundMode(a1); }
};
