#pragma once

#include <QtQml>

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

