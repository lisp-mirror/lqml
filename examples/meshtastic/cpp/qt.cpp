#include "qt.h"
#include "ble_meshtastic.h"
#include <ecl_fun.h>
#include <QSqlQuery>
#include <QSqlError>
#include <QNetworkInterface>
#include <QHostAddress>
#include <QtDebug>

#ifdef Q_OS_ANDROID
  #include <QtAndroid>
  #include <QAndroidJniEnvironment>
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
  // BLE
  ble = new BLE_ME;
  ble->connect(ble, &BLE::deviceDiscovered,
               [](const QString& fullName) {
                 ecl_fun("radios:device-discovered", fullName.right(4));
               });
  ble->connect(ble, &BLE::bleError,
               []() {
                 ecl_fun("radios:reset-default-radio");
               });
}

// BLE_ME

QVariant QT::startDeviceDiscovery(const QVariant& vName) {
  auto name = vName.toString();
  if (!name.isNull()) {
    ble->initialDeviceName = name;
  }
  ble->startDeviceDiscovery();
  return vName;
}

QVariant QT::setDeviceFilter(const QVariant& vName) {
  ble->nameFilter = vName.toString();
  return vName;
}

QVariant QT::shortNames() {
  QVariantList names;
  for (auto device : qAsConst(ble->devices)) {
    names << device.name().right(4);
  }
  return names;
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
  QSqlQuery query(db);
  if (db.open()) {
    query.prepare(vQuery.toString());
    const QVariantList values = vValues.value<QVariantList>();
    for (auto value : values) {
      query.addBindValue(value);
    }
    if (query.exec()) {
      auto rows = vRows.toInt();
      while (query.next()) {
        if (rows > 1) {
          QVariantList list;
          for (auto r = 0; r < rows; r++) {
            list << query.value(r);
          }
          results << QVariant(list);
        } else {
          results << query.value(0);
        }
      }
      db.close();
      return results;
    }
    db.close();
  }
  QString text;
  if (query.lastError().isValid()) {
    text = query.lastError().text();
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
