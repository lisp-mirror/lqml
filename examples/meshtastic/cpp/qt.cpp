#include "qt.h"
#include <QSqlQuery>
#include <QSqlRecord>
#include <QSqlError>
#include <QNetworkInterface>
#include <QHostAddress>
#include <QGuiApplication>
#include <QtDebug>

#ifdef PLUGIN
  #include <ecl_fun_plugin.h>
#else
  #include <ecl_fun.h>
#endif

#ifdef Q_OS_ANDROID
  QT* QT::_this = nullptr;
  #define CON QtAndroidServiceReplica
  #include "rep_qtandroidservice_replica.h"
  #if (QT_VERSION < 0x060000)
    #include <QtAndroid>
    #include <QAndroidJniEnvironment>
  #else
    #include <QtCore/private/qandroidextras_p.h>
  #endif
#else
  #define CON Connection
  #include "connection/connection.h"
#endif

QObject* ini() {
  static QObject* qt = nullptr;
  if (qt == nullptr) {
    qt = new QT;
  }
  return qt;
}

// connection

QT::QT() : QObject() {
#ifdef Q_OS_ANDROID
  _this = this;
  iniJni();

  // remote object for android service
  QRemoteObjectNode* repNode = new QRemoteObjectNode;
  repNode->connectToNode(QUrl(QStringLiteral("local:replica")));
  con = repNode->acquire<QtAndroidServiceReplica>();
  bool res = con->waitForSource();
  Q_ASSERT(res);
#else
  con = new Connection;
#endif

  QObject::connect(con, &CON::deviceDiscovered,
    [](const QVariant& vName) {
      ecl_fun("radios:device-discovered", vName.toString().right(4)); // short name
    });

  QObject::connect(con, &CON::bleError,
    []() {
      ecl_fun("radios:reset");
    });

  QObject::connect(con, &CON::setReady,
    [](const QVariant& vArg) {
      ecl_fun("lora:set-ready", vArg);
    });

  QObject::connect(con, &CON::sendingDone,
    []() {
      ecl_fun("lora:send-enqueued");
    });

  QObject::connect(con, &CON::receivedFromRadio,
    [](const QVariant& vArg) {
      ecl_fun("lora:received-from-radio", vArg);
    });

  QObject::connect(con, &CON::receivingDone,
    []() {
      ecl_fun("lora:receiving-done");
    });

  QObject::connect(con, &CON::sendSavedPackets,
    [](const QVariant& vPackets) {
      ecl_fun("lora:process-saved-packets", vPackets);
    });

#if (defined Q_OS_ANDROID) || (defined Q_OS_IOS)
  // background mode
  QObject::connect(qGuiApp, &QGuiApplication::applicationStateChanged,
    [&](Qt::ApplicationState state) {
      static bool ok = false; // for startup
      if (state == Qt::ApplicationInactive) {
        ok = true;
        con->setBackgroundMode(true);
        ecl_fun("app:background-mode-changed", true);
      } else if (state == Qt::ApplicationActive) {
        if (ok) {
           con->setBackgroundMode(false);
           ecl_fun("app:background-mode-changed", false);
        }
      }
    });
#endif
}

QVariant QT::setConnectionType(const QVariant& vType) {
  con->setConnectionType(vType);
  return QVariant();
}

QVariant QT::startDeviceDiscovery(const QVariant& vName) {
  con->startDeviceDiscovery(vName);
  return QVariant();
}

QVariant QT::stopDeviceDiscovery() {
  con->stopDeviceDiscovery();
  return QVariant();
}

QVariant QT::disconnect() {
  con->disconnect();
  return QVariant();
}

QVariant QT::setDeviceFilter(const QVariant& vName) {
  con->setDeviceFilter(vName);
  return QVariant();
}

QVariant QT::read2() {
  con->read2();
  return QVariant();
}

QVariant QT::write2(const QVariant& vBytes) {
  con->write2(vBytes);
  return QVariant();
}

QVariant QT::wifiConnectable(const QVariant& vIP) {
  if (tcp.state() == QTcpSocket::ConnectedState) {
    return true;
  }
  tcp.connectToHost(vIP.toString(), 4403);
  if (tcp.waitForConnected(1000)) {
    return true;
  }
  return QVariant();
}

QVariant QT::hasFeature(const QVariant& vName) {
  auto name = vName.toString().toLower();
#ifndef NO_BLE
  if (name == "ble") return true;
#endif
#ifndef NO_USB
  if (name == "usb") return true;
#endif
  return QVariant();
}

// SQLite

QVariant QT::iniDb(const QVariant& vName) {
  db = QSqlDatabase::addDatabase("QSQLITE");
  db.setDatabaseName(vName.toString());
  return vName;
}

QVariant QT::sqlQuery(const QVariant& vQuery, const QVariant& vValues) {
  QVariantList results;
  QSqlQuery sqlQuery(db);
  if (db.open()) {
    QString query = vQuery.toString();
    sqlQuery.prepare(vQuery.toString());
    const QVariantList values = vValues.value<QVariantList>();
    for (auto value : values) {
      sqlQuery.addBindValue(value);
    }
    if (sqlQuery.exec()) {
      auto cols = sqlQuery.record().count();
      while (sqlQuery.next()) {
        if (cols > 1) {
          QVariantList list;
          for (auto c = 0; c < cols; c++) {
            list << sqlQuery.value(c);
          }
          results << QVariant(list);
        } else {
          results << sqlQuery.value(0);
        }
      }
      if (!cols && query.startsWith("insert", Qt::CaseInsensitive)) {
        results << sqlQuery.lastInsertId();
      }
      db.close();
      return results;
    }
    db.close();
  }
  QString text;
  if (sqlQuery.lastError().isValid()) {
    text = sqlQuery.lastError().text();
  } else {
    text = db.lastError().text();
  }
  qDebug() << "SQL error:" << text;
  return QVariant();
}

// etc

QVariant QT::dataPath(const QVariant& prefix) {
  // for desktop
  QString path = QStandardPaths::writableLocation(QStandardPaths::AppLocalDataLocation);
  path.truncate(path.lastIndexOf(QChar('/')));
  path.append(QStringLiteral("/cl-meshtastic/") + prefix.toString());
  return path;
}

QVariant QT::localIp() {
  // returns the local IP string; private networks may use:
  // 10.*.*.*
  // 172.16.*.*
  // 192.168.*.*
  const auto addresses = QNetworkInterface::allAddresses();
  QStringList ips;
  for (QHostAddress adr : addresses) {
    if (adr.protocol() == QAbstractSocket::IPv4Protocol) {
      QString ip(adr.toString());
      if (ip.startsWith("10.") ||
          ip.startsWith("172.16.") ||
          ip.startsWith("192.168.")) {
        ips << ip;
      }
    }
  }
  if (!ips.isEmpty()) {
    // hack for rare, ambiguous cases
    ips.sort();
    return ips.first();
  }
  return QVariant();
}

