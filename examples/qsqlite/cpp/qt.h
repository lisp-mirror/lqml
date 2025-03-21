#pragma once

#include <QtCore>
#include <QSqlDatabase>
#include <QQuickImageProvider>

#ifdef Q_CC_MSVC
#define LIB_EXPORT __declspec(dllexport)
#else
#define LIB_EXPORT
#endif

extern "C" { LIB_EXPORT QObject* ini(); }

class QT : public QObject {
  Q_OBJECT

public:
  Q_INVOKABLE QVariant dataPath(const QVariant&);
  Q_INVOKABLE QVariant iniDb(const QVariant&, const QVariant&);
  Q_INVOKABLE QVariant sqlQuery(const QVariant&, const QVariant&, const QVariant&);

  QT();

  QSqlDatabase db;
};

class DatabaseImageProvider : public QQuickImageProvider {

public:
  DatabaseImageProvider(QT* _qt) : QQuickImageProvider(QQuickImageProvider::Pixmap), qt(_qt) {}

  QPixmap requestPixmap(const QString&, QSize*, const QSize&);

  QT* qt;
};
