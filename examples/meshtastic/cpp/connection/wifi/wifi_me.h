#pragma once

#include <QTcpSocket>
#include <QTimer>

#ifdef Q_OS_ANDROID
class QtAndroidService;
#endif
class Connection;

class WiFi_ME : public QTcpSocket {
  Q_OBJECT

  /*** <INTERFACE> ****************************************/

public:
#ifdef Q_OS_ANDROID
  WiFi_ME(QtAndroidService*, Connection*);
#else
  WiFi_ME(Connection*);
#endif

public Q_SLOTS:
  void connectToRadio(const QString&);
  void disconnect();
  void read2();
  void write2(const QByteArray&);

Q_SIGNALS:
  void setReady(const QString&);
  void receivedFromRadio(const QByteArray&, const QString&);
  void receivingDone();

  /*** </INTERFACE> ***************************************/

public:
#ifdef Q_OS_ANDROID
  QtAndroidService* emitter = nullptr;
#else
  Connection* emitter = nullptr;
#endif
  Connection* con = nullptr;
  QTimer timer;
  QByteArrayList packets;

  void received(const QByteArray&);

public Q_SLOTS:
  void done();
  void stateChanged(SocketState);
};
