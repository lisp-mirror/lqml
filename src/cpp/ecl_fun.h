// header to be included in external Qt libraries
// for calling ECL functions from Qt

#pragma once

#include <QVariant>

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

