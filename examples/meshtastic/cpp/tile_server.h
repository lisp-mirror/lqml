#pragma once

#include <QTcpServer>
#include <QTcpSocket>

// trivial local tile web-server which doesn't need an API key

class TileServer : public QTcpServer {
  Q_OBJECT

public:
  int port;

  TileServer(int p = 0, QObject* parent = nullptr) : QTcpServer(parent) {
    listen(QHostAddress::Any, p);
    port = serverPort();
  }

  void incomingConnection(qintptr socket) override {
    QTcpSocket* s = new QTcpSocket(this);
    connect(s, &QTcpSocket::readyRead, this, &TileServer::readClient);
    connect(s, &QTcpSocket::disconnected, this, &TileServer::discardClient);
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

