// header to be included in external Qt libraries
// for calling ECL functions from Qt

#pragma once

#include <QVariant>

QT_BEGIN_NAMESPACE

extern QVariant ecl_fun(
  const QByteArray&,
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant(),
  const QVariant& = QVariant());

QT_END_NAMESPACE
