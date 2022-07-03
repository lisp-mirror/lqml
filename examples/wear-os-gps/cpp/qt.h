#pragma once

#include <QObject>
#include <QVariant>

QT_BEGIN_NAMESPACE

extern "C" { QObject* ini(); }

class QT : public QObject {
  Q_OBJECT

public:
  Q_INVOKABLE QVariant keepScreenOn();
};

QT_END_NAMESPACE
