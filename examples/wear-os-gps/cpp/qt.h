#pragma once

#include <QObject>
#include <QVariant>

extern "C" { QObject* ini(); }

class QT : public QObject {
  Q_OBJECT

public:
  Q_INVOKABLE QVariant keepScreenOn(const QVariant& = true);
};

