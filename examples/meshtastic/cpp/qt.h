#pragma once

#include <QtCore>
#include <QSqlDatabase>

#ifdef Q_CC_MSVC
#define LIB_EXPORT __declspec(dllexport)
#else
#define LIB_EXPORT
#endif

#ifdef Q_OS_ANDROID
  class QtAndroidServiceReplica;
#else
  class BLE_ME;
#endif

QT_BEGIN_NAMESPACE

extern "C" { LIB_EXPORT QObject* ini(); }

class QT : public QObject {
  Q_OBJECT

public:
  // BLE
  Q_INVOKABLE QVariant startDeviceDiscovery(const QVariant&);
  Q_INVOKABLE QVariant setDeviceFilter(const QVariant&);
  Q_INVOKABLE QVariant read2();
  Q_INVOKABLE QVariant write2(const QVariant&);
  Q_INVOKABLE QVariant setBackgroundMode(const QVariant&);

  // GPS
  Q_INVOKABLE QVariant iniPositioning();
  Q_INVOKABLE QVariant lastPosition();

  // SQLite
  Q_INVOKABLE QVariant iniDb(const QVariant&);
  Q_INVOKABLE QVariant sqlQuery(const QVariant&, const QVariant&, const QVariant&);

  // etc
  Q_INVOKABLE QVariant dataPath(const QVariant&);
  Q_INVOKABLE QVariant localIp();
#if (defined Q_OS_ANDROID) || (defined Q_OS_IOS)
  Q_INVOKABLE QVariant keepScreenOn(const QVariant& = true);
#endif

  QT();

  QSqlDatabase db;
#ifdef Q_OS_ANDROID
  QSharedPointer<QtAndroidServiceReplica> ble;
#else
  BLE_ME* ble;
#endif
};

QT_END_NAMESPACE
