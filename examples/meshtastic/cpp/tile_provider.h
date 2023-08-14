#pragma once

#include <QTcpServer>
#include <QTcpSocket>
#include <QtDebug>

// trivial local tile provider which doesn't need an API key
// (default Qt provider requires an API key from thunderforest.com)

class TileProvider : public QTcpServer {
  Q_OBJECT

public:
  TileProvider(int port = 0, QObject* parent = nullptr) : QTcpServer(parent) {
    listen(QHostAddress::Any, port);
    qDebug() << "tile provider started at IP" << serverAddress() << "port" << serverPort();
  }

  void incomingConnection(qintptr socket) override {
    QTcpSocket* s = new QTcpSocket(this);
    connect(s, &QTcpSocket::readyRead, this, &TileProvider::readClient);
    connect(s, &QTcpSocket::disconnected, this, &TileProvider::discardClient);
    s->setSocketDescriptor(socket);
  }

public Q_SLOTS:
  void readClient() {
    QString xml = QStringLiteral(
    "{\"UrlTemplate\" : \"https://tile.openstreetmap.org/%z/%x/%y.png\","
    " \"ImageFormat\" : \"png\","
    " \"QImageFormat\" : \"Indexed8\","
    " \"ID\" : \"wmf-intl-1x\","
    " \"MaximumZoomLevel\" : 19,"
    " \"MapCopyRight\" : \"<a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>\","
    " \"DataCopyRight\" : \"\"}");

    QTcpSocket* socket = static_cast<QTcpSocket*>(sender());
    if (socket->canReadLine()) {
      QString line = socket->readLine();
      QStringList tokens = line.split(QRegExp("[ \r\n][ \r\n]*"));
      if (tokens.at(0) == "GET") {
        QTextStream s(socket);
        s.setCodec("UTF-8");
        s << QStringLiteral("HTTP/1.0 200 Ok\r\n"
                            "Content-Type: text/html; charset=\"utf-8\"\r\n\r\n")
          << xml
          << QStringLiteral("\r\n");
        socket->close();

        if (socket->state() == QTcpSocket::UnconnectedState) {
          delete socket;
        }
      }
    }
  }

  void discardClient() {
    QTcpSocket* socket = static_cast<QTcpSocket*>(sender());
    socket->deleteLater();
  }
};

