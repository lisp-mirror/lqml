#pragma once

#include <QtCore>

#ifdef Q_CC_MSVC
#define LIB_EXPORT __declspec(dllexport)
#else
#define LIB_EXPORT
#endif

QT_BEGIN_NAMESPACE

extern "C" { LIB_EXPORT QObject* ini(); }

class QT : public QObject {
  Q_OBJECT

public:
  Q_INVOKABLE QVariant rotateImage(const QVariant&, const QVariant&);
};

QT_END_NAMESPACE
