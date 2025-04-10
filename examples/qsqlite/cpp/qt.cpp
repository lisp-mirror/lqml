#include "qt.h"
#include <QSqlQuery>
#include <QSqlRecord>
#include <QSqlError>
#include <QQuickView>
#include <QtDebug>

QObject* ini() {
  static QObject* qt = nullptr;
  if (qt == nullptr) {
    qt = new QT;
  }
  return qt;
}

QT::QT() : QObject() {
}

QVariant QT::dataPath(const QVariant& prefix) {
  // for desktop
  QString path = QStandardPaths::writableLocation(QStandardPaths::AppLocalDataLocation);
  path.truncate(path.lastIndexOf(QChar('/')));
  path.append(QStringLiteral("/lqml-qsqlite/") + prefix.toString());
  return path;
}

// SQL

QVariant QT::iniDb(const QVariant& vName, const QVariant& vQuickView) {
  // add database image provider, in order to load images in QML directly from a SQL database
  auto quickView = vQuickView.value<QQuickView*>();
  quickView->engine()->addImageProvider(QLatin1String("db"), new DatabaseImageProvider(this));
  // ini
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

// database image provider

QPixmap DatabaseImageProvider::requestPixmap(const QString& name, QSize* size, const QSize& requestedSize) {
  auto result = qt->sqlQuery(
    "select data from images where name = ?",
    QVariantList() << name).value<QVariantList>();
  QPixmap pixmap;
  if (!result.isEmpty()) {
    pixmap.loadFromData(result.first().toByteArray());
    *size = pixmap.size();
    if (requestedSize.isValid() && (pixmap.size() != requestedSize)) {
      pixmap = pixmap.scaled(requestedSize);
    }
  }
  return pixmap;
}

