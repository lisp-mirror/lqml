#include "qt.h"
#include <QSqlQuery>
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
  #include "rep_qtandroidservice_replica.h"
  #if (QT_VERSION < 0x060000)
    #include <QtAndroid>
    #include <QAndroidJniEnvironment>
  #else
    #include <QtCore/private/qandroidextras_p.h>
  #endif
#else
  #include "connection/connection.h"
#endif

QT_BEGIN_NAMESPACE

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
  // remote object for android service
  QRemoteObjectNode* repNode = new QRemoteObjectNode;
  repNode->connectToNode(QUrl(QStringLiteral("local:replica")));
  con = repNode->acquire<QtAndroidServiceReplica>();
  bool res = con->waitForSource();
  Q_ASSERT(res);
#else
  con = new Connection;
#endif

#ifdef Q_OS_ANDROID
  QObject::connect(con, &QtAndroidServiceReplica::deviceDiscovered,
#else
  QObject::connect(con, &Connection::deviceDiscovered,
#endif
  [](const QVariant& vFullName) {
    ecl_fun("radios:device-discovered", vFullName.toString().right(4));
  });

#ifdef Q_OS_ANDROID
  QObject::connect(con, &QtAndroidServiceReplica::bleError,
#else
  QObject::connect(con, &Connection::bleError,
#endif
  []() {
    ecl_fun("radios:reset");
  });

#ifdef Q_OS_ANDROID
  QObject::connect(con, &QtAndroidServiceReplica::setReady,
#else
  QObject::connect(con, &Connection::setReady,
#endif
  [](const QVariant& vArg) {
    ecl_fun("lora:set-ready", vArg);
  });

#ifdef Q_OS_ANDROID
  QObject::connect(con, &QtAndroidServiceReplica::receivedFromRadio,
#else
  QObject::connect(con, &Connection::receivedFromRadio,
#endif
  [](const QVariant& vArg) {
    ecl_fun("lora:received-from-radio", vArg);
  });

#ifdef Q_OS_ANDROID
  QObject::connect(con, &QtAndroidServiceReplica::receivingDone,
#else
  QObject::connect(con, &Connection::receivingDone,
#endif
  []() {
    ecl_fun("lora:receiving-done");
  });

#ifdef Q_OS_ANDROID
  QObject::connect(con, &QtAndroidServiceReplica::sendSavedPackets,
#else
  QObject::connect(con, &Connection::sendSavedPackets,
#endif
  [](const QVariant& vPackets) {
    ecl_fun("lora:process-saved-packets", vPackets);
  });

#if (defined Q_OS_ANDROID) || (defined Q_OS_IOS)
  // background mode
  QObject::connect(qGuiApp, &QGuiApplication::applicationStateChanged,
                   [&](Qt::ApplicationState state) {
                     if (state == Qt::ApplicationInactive) {
                       con->setBackgroundMode(true);
                       ecl_fun("app:background-mode-changed", true);
                     } else if (state == Qt::ApplicationActive) {
                       static bool startup = true;
                       if (startup) {
                         startup = false;
                       } else {
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

// GPS

#ifdef Q_OS_ANDROID
static void clearEventualExceptions() {
#if (QT_VERSION < 0x060000)
  QAndroidJniEnvironment env;
#else
  QJniEnvironment env;
#endif
  if (env->ExceptionCheck()) {
    env->ExceptionClear();
  }
}

static qlonglong getLongField(const char* name) {
#if (QT_VERSION < 0x060000)
  QAndroidJniObject activity = QtAndroid::androidActivity();
#else
  QJniObject activity = QtAndroidPrivate::activity();
#endif
  return static_cast<qlonglong>(activity.getField<jlong>(name));
}

static double getDoubleField(const char* name) {
#if (QT_VERSION < 0x060000)
  QAndroidJniObject activity = QtAndroid::androidActivity();
#else
  QJniObject activity = QtAndroidPrivate::activity();
#endif
  return static_cast<double>(activity.getField<jdouble>(name));
}
#endif

QVariant QT::iniPositioning() {
#ifdef Q_OS_ANDROID
  #if (QT_VERSION < 0x060000)
  QtAndroid::runOnAndroidThread([] {
    QAndroidJniObject activity = QtAndroid::androidActivity();
    activity.callMethod<void>("iniLocation", "()V");
    clearEventualExceptions();
  });
  #else
  QNativeInterface::QAndroidApplication::runOnAndroidMainThread([&] {
    QJniObject activity = QtAndroidPrivate::activity();
    activity.callMethod<void>("iniLocation", "()V");
    clearEventualExceptions();
  });
  #endif
#endif
  return QVariant();
}

QVariant QT::lastPosition() {
  QVariantList pos;
#ifdef Q_OS_ANDROID
  pos << getDoubleField("_position_lat_")
      << getDoubleField("_position_lon_")
      << getLongField("_position_time_");
#endif
  return pos;
}

// SQLite

QVariant QT::iniDb(const QVariant& name) {
  db = QSqlDatabase::addDatabase("QSQLITE");
  db.setDatabaseName(name.toString());
  return name;
}

QVariant QT::sqlQuery(const QVariant& vQuery, const QVariant& vValues, const QVariant& vRows) {
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
      auto rows = vRows.toInt();
      while (sqlQuery.next()) {
        if (rows > 1) {
          QVariantList list;
          for (auto r = 0; r < rows; r++) {
            list << sqlQuery.value(r);
          }
          results << QVariant(list);
        } else {
          results << sqlQuery.value(0);
        }
      }
      if (!rows && query.startsWith("insert", Qt::CaseInsensitive)) {
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

QT_END_NAMESPACE
