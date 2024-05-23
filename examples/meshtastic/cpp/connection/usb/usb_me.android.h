#pragma once

#include <QTimer>

class Connection;
class QtAndroidService;

class USB_ME : public QObject {
  Q_OBJECT

public:
  USB_ME(QtAndroidService*, Connection*);

public Q_SLOTS:
  void connectToRadio();
  void disconnect();
  void read2();
  void write2(const QByteArray&);

public:
  static USB_ME* _this;

  bool ready = false;
  QtAndroidService* emitter = nullptr;
  Connection* con = nullptr;
  QTimer timer;
  QByteArrayList packets;

  void received(const QByteArray&);

  Q_INVOKABLE void onNewData(const QByteArray&);

public Q_SLOTS:
  void done();
};

