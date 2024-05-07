#pragma once

#include <QSerialPort>
#include <QTimer>

class USB : public QSerialPort {
  Q_OBJECT

public:
  USB();

  bool ready = false;
  QTimer timer;
  QByteArrayList packets;

  void received(const QByteArray&);

public Q_SLOTS:
  bool connect2();
  void disconnect();
  void read2();
  void write2(const QByteArray&);
  void done();

Q_SIGNALS:
  void setReady(const QString&);
  void receivingDone(const QByteArray&);
};

