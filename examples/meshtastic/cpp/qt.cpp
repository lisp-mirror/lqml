#include "qt.h"
#include <QSqlQuery>
#include <QSqlError>
#include <QNetworkInterface>
#include <QHostAddress>
#include <QtDebug>

#ifdef PLUGIN
  #include <ecl_fun_plugin.h>
#else
  #include <ecl_fun.h>
#endif

#ifdef Q_OS_ANDROID
  #include "rep_qtandroidservice_replica.h"
  #include <QtAndroid>
  #include <QAndroidIntent>
  #include <QAndroidJniEnvironment>
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

#ifdef Q_OS_ANDROID
static void startService() {
  QAndroidIntent serviceIntent(QtAndroid::androidActivity().object(),
                               "org/cl/meshtastic/QtAndroidService");
  QAndroidJniObject result = QtAndroid::androidActivity().callObjectMethod(
    "startService",
    "(Landroid/content/Intent;)Landroid/content/ComponentName;",
    serviceIntent.handle().object());
}
#endif

QT::QT() : QObject() {
#ifdef Q_OS_ANDROID
  // android service using remote object
  startService();

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
    ecl_fun("radios:reset-default-radio");
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
    ecl_fun("lora:received-from-radio", data, notified);
  });

#ifdef Q_OS_ANDROID
  QObject::connect(ble.data(), &QtAndroidServiceReplica::receivingDone,
#else
  QObject::connect(ble, &BLE_ME::receivingDone,
#endif
  []() {
    ecl_fun("lora:receiving-done");
  });
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

// GPS

#ifdef Q_OS_ANDROID
static void clearEventualExceptions() {
  QAndroidJniEnvironment env;
  if (env->ExceptionCheck()) {
    env->ExceptionClear();
  }
}

static qlonglong getLongField(const char* name) {
  QAndroidJniObject activity = QtAndroid::androidActivity();
  return static_cast<qlonglong>(activity.getField<jlong>(name));
}

static double getDoubleField(const char* name) {
  QAndroidJniObject activity = QtAndroid::androidActivity();
  return static_cast<double>(activity.getField<jdouble>(name));
}
#endif

QVariant QT::iniPositioning() {
#ifdef Q_OS_ANDROID
  QtAndroid::runOnAndroidThread([] {
    QAndroidJniObject activity = QtAndroid::androidActivity();
    activity.callMethod<void>("iniLocation", "()V");
    clearEventualExceptions();
  });
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
