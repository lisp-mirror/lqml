#include "qt.h"
#include "ble_meshtastic.h"
#include <QSqlQuery>
#include <QSqlError>
#include <QtDebug>

QT_BEGIN_NAMESPACE

QObject* ini() {
  static QObject* qt = nullptr;
  if (qt == nullptr) {
    qt = new QT;
  }
  return qt;
}

QT::QT() : QObject() {
  ble = new BLE_ME;
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

// SQLite

QVariant QT::iniDb(const QVariant& name) {
  db = QSqlDatabase::addDatabase("QSQLITE");
  db.setDatabaseName(name.toString());
  return name;
}

QVariant QT::sqlQuery(const QVariant& vQuery, const QVariant& vValues) {
  // very simple, we don't need more
  QVariantList results;
  QSqlQuery query(db);
  if (db.open()) {
    query.prepare(vQuery.toString());
    const QVariantList values = vValues.value<QVariantList>();
    for (auto value : values) {
      query.addBindValue(value);
    }
    if (query.exec()) {
      while (query.next()) {
        results << query.value(0);
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

QT_END_NAMESPACE
