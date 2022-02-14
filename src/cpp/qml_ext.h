#ifndef QML_EXT_H
#define QML_EXT_H

#include <QtQml>

QT_BEGIN_NAMESPACE

QObject* iniQml();

class Lisp : public QObject {
  Q_OBJECT

public:
  Q_INVOKABLE QVariant call(
    const QJSValue&,
    const QJSValue& = QJSValue(),
    const QJSValue& = QJSValue(),
    const QJSValue& = QJSValue(),
    const QJSValue& = QJSValue(),
    const QJSValue& = QJSValue(),
    const QJSValue& = QJSValue(),
    const QJSValue& = QJSValue(),
    const QJSValue& = QJSValue(),
    const QJSValue& = QJSValue(),
    const QJSValue& = QJSValue(),
    const QJSValue& = QJSValue(),
    const QJSValue& = QJSValue(),
    const QJSValue& = QJSValue(),
    const QJSValue& = QJSValue(),
    const QJSValue& = QJSValue(),
    const QJSValue& = QJSValue(),
    const QJSValue& = QJSValue());

  Q_INVOKABLE QVariant apply(
    const QJSValue&,
    const QJSValue& = QJSValue(),
    const QJSValue& = QJSValue());
};

QT_END_NAMESPACE

#endif
