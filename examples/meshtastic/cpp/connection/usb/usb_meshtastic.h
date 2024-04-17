#pragma once

#include <QSerialPort>

#ifdef Q_OS_ANDROID
class QtAndroidService;
#endif
class Connection;

class USB_ME : public QSerialPort {
  Q_OBJECT

  /*** <INTERFACE> ****************************************/

public:
#ifdef Q_OS_ANDROID
  USB_ME(QtAndroidService*, Connection*);
#else
  USB_ME(Connection*);
#endif

public Q_SLOTS:
  void connectToRadio();
  void disconnect();
  void read2();
  void write2(const QByteArray&);

Q_SIGNALS:
  void setReady(const QString&);
  void receivedFromRadio(const QByteArray&);
  void receivingDone();

  /*** </INTERFACE> ***************************************/

public:
#ifdef Q_OS_ANDROID
  QtAndroidService* emitter = nullptr;
#else
  Connection* emitter = nullptr;
#endif
  Connection* con = nullptr;
  bool ready = false;

  void received(const QByteArray&);

public Q_SLOTS:
  void done();
};

