#pragma once

#include <QSerialPort>
#include <QTimer>

class Connection;

class USB_ME : public QSerialPort {
  Q_OBJECT

public:
  USB_ME(Connection*);

public Q_SLOTS:
  void connectToRadio();
  void disconnect();
  void read2();
  void write2(const QByteArray&);

public:
  bool ready = false;
  Connection* con = nullptr;
  QTimer timer;
  QByteArrayList packets;

  void received(const QByteArray&);

public Q_SLOTS:
  void done();
};

