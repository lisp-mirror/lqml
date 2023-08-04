#pragma once

#include <QtCore>
#include <QSqlDatabase>

#ifdef Q_CC_MSVC
#define LIB_EXPORT __declspec(dllexport)
#else
#define LIB_EXPORT
#endif

class BLE_ME;

QT_BEGIN_NAMESPACE

extern "C" { LIB_EXPORT QObject* ini(); }

class QT : public QObject {
  Q_OBJECT

public:
  // BLE_ME
  Q_INVOKABLE QVariant startDeviceDiscovery(const QVariant&);
  Q_INVOKABLE QVariant shortNames();
  Q_INVOKABLE QVariant read2();
  Q_INVOKABLE QVariant write2(const QVariant&);

  // GPS
  Q_INVOKABLE QVariant iniPositioning();
  Q_INVOKABLE QVariant lastPosition();

  // SQLite
  Q_INVOKABLE QVariant iniDb(const QVariant&);
  Q_INVOKABLE QVariant sqlQuery(const QVariant&, const QVariant&);

  // etc
  Q_INVOKABLE QVariant localIp();
  Q_INVOKABLE QVariant startTileServer();

  QT();

  BLE_ME* ble;
  QSqlDatabase db;
};

QT_END_NAMESPACE
