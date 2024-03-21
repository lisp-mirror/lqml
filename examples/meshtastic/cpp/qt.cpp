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
  #include "ble/ble_meshtastic.h"
#endif

QT_BEGIN_NAMESPACE

QObject* ini() {
  static QObject* qt = nullptr;
  if (qt == nullptr) {
    qt = new QT;
  }
  return qt;
}

QT::QT() : QObject() {
#ifdef Q_OS_ANDROID
  // remote object for android service
  QRemoteObjectNode* repNode = new QRemoteObjectNode;
  repNode->connectToNode(QUrl(QStringLiteral("local:replica")));
  QSharedPointer<QtAndroidServiceReplica> rep(repNode->acquire<QtAndroidServiceReplica>());
  bool res = rep->waitForSource();
  Q_ASSERT(res);

  ble = rep;
#else
  ble = new BLE_ME;
#endif

#ifdef Q_OS_ANDROID
  QObject::connect(ble.data(), &QtAndroidServiceReplica::deviceDiscovered,
#else
  QObject::connect(ble, &BLE::deviceDiscovered,
#endif
  [](const QString& fullName) {
    ecl_fun("radios:device-discovered", fullName.right(4));
  });

#ifdef Q_OS_ANDROID
  QObject::connect(ble.data(), &QtAndroidServiceReplica::bleError,
#else
  QObject::connect(ble, &BLE::bleError,
#endif
  []() {
    ecl_fun("radios:reset");
  });

#ifdef Q_OS_ANDROID
  QObject::connect(ble.data(), &QtAndroidServiceReplica::setReady,
#else
  QObject::connect(ble, &BLE_ME::setReady,
#endif
  [](bool ready, const QString& current, const QStringList& names) {
    QVariantList vNames; // Lisp doesn't know 'QStringList'
    for (auto name : names) {
      vNames << name;
    }
    ecl_fun("lora:set-ready", ready, current, vNames);
  });

#ifdef Q_OS_ANDROID
  QObject::connect(ble.data(), &QtAndroidServiceReplica::receivedFromRadio,
#else
  QObject::connect(ble, &BLE_ME::receivedFromRadio,
#endif
  [](const QByteArray& data, const QString& notified) {
    ecl_fun("lora:received-from-radio", data, notified.isEmpty() ? QVariant() : notified);
  });

#ifdef Q_OS_ANDROID
  QObject::connect(ble.data(), &QtAndroidServiceReplica::receivingDone,
#else
  QObject::connect(ble, &BLE_ME::receivingDone,
#endif
  []() {
    ecl_fun("lora:receiving-done");
  });

#ifdef Q_OS_ANDROID
  QObject::connect(ble.data(), &QtAndroidServiceReplica::sendSavedPackets,
#else
  QObject::connect(ble, &BLE_ME::sendSavedPackets,
#endif
  [](const QVariant& packets) {
    ecl_fun("lora:process-saved-packets", packets);
  });

#if (defined Q_OS_ANDROID) || (defined Q_OS_IOS)
  // background mode
  QObject::connect(qGuiApp, &QGuiApplication::applicationStateChanged,
                   [&](Qt::ApplicationState state) {
                     if (state == Qt::ApplicationInactive) {
                       ble->setBackgroundMode(true);
                       ecl_fun("app:background-mode-changed", true);
                     } else if (state == Qt::ApplicationActive) {
                       static bool startup = true;
                       if (startup) {
                         startup = false;
                       } else {
                         ble->setBackgroundMode(false);
                         ecl_fun("app:background-mode-changed", false);
                       }
                     }
                   });
#endif
}

// BLE

QVariant QT::startDeviceDiscovery(const QVariant& vName) {
  ble->startDeviceDiscovery(vName.toString());
  return vName;
}

QVariant QT::setDeviceFilter(const QVariant& vName) {
  ble->setDeviceFilter(vName.toString());
  return vName;
}

QVariant QT::read2() {
  ble->read();
  return QVariant();
}

QVariant QT::write2(const QVariant& bytes) {
  ble->write(bytes.toByteArray());
  return QVariant();
}

QVariant QT::setBackgroundMode(const QVariant& vBackground) {
  // for testing
  ble->setBackgroundMode(vBackground.toBool());
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
  // Returns the local IP string. Private networks may use:
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
