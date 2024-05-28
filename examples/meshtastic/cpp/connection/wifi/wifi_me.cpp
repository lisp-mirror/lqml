#include "wifi_me.h"
#include "../connection.h"
#include <QHostAddress>

#ifdef Q_OS_ANDROID
  #include "../../android_service/qtandroidservice_ro.h"
#endif

#ifdef Q_OS_ANDROID
WiFi_ME::WiFi_ME(QtAndroidService* service, Connection* _con) : emitter(service), con(_con) {
#else
WiFi_ME::WiFi_ME(Connection* _con) : emitter(_con), con(_con) {
#endif
  connect(this, &QTcpSocket::stateChanged, this, &WiFi_ME::stateChanged);
  connect(this, &QTcpSocket::errorOccurred,
          [](QAbstractSocket::SocketError error) {
            qDebug() << "WiFi error:" << error;
          });
  connect(this, &QTcpSocket::readyRead, this, &WiFi_ME::read2);
  connect(this, &QIODevice::bytesWritten, [&](qint64) { emitter->sendingDone(); });

  timer.setSingleShot(true);
  connect(&timer, &QTimer::timeout, this, &WiFi_ME::done);
}

void WiFi_ME::connectToRadio(const QString& ip) {
  if (state() != ConnectedState) {
    connectToHost(ip, 4403);
  }
}

void WiFi_ME::stateChanged(SocketState state) {
  if (state == ConnectedState) {
    emitter->setReady(QVariant(QVariantList() << peerAddress().toString()));
  }
  qDebug() << "WiFi state:" << state;
}

void WiFi_ME::disconnect() {
  if (state() == ConnectedState) {
    disconnectFromHost();
  }
}

void WiFi_ME::write2(const QByteArray& data) {
  if (state() == ConnectedState) {
    write(data);
  } else {
    qDebug() << "WiFi not ready: write()";
  }
}

void WiFi_ME::read2() {
  packets << readAll();
  timer.start(1000); // assume receiving done after pause of 1 sec
}

void WiFi_ME::received(const QByteArray& data) {
  if (!data.isEmpty()) {
    if (con->backgroundMode) {
      con->saveBytes(data);
    } else {
      emitter->receivedFromRadio(QVariant(QVariantList() << data));
    }
  }
}

void WiFi_ME::done() {
  if (!con->backgroundMode) {
    static bool startup = true;
    if (startup) {
      con->sendSavedBytes(); // for eventual, saved but not sent packets
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

