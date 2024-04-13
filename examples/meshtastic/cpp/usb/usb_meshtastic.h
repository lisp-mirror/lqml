#pragma once

#include <QSerialPort>

class USB_ME : public QSerialPort {
  Q_OBJECT

public:
  USB_ME();

  enum State {
    Closed,
    Open,
    Ready
  };

  State state = Closed;

public Q_SLOTS:
  void wantConfigId();
  void write2(const QByteArray&);
  void read2();

Q_SIGNALS:
  void setReady(const QString&);
  void receivedFromRadio(const QByteArray&);
  void receivingDone();
};

