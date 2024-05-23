#pragma once

#include <QtCore>

#ifdef Q_CC_MSVC
#define LIB_EXPORT __declspec(dllexport)
#else
#define LIB_EXPORT
#endif

extern "C" { LIB_EXPORT QObject* ini(); }

class CPP : public QObject {
  Q_OBJECT

public:
  // max. 10 arguments of type QVariant
  // return type must also be a QVariant

  Q_INVOKABLE QVariant hello(const QVariant&);
  Q_INVOKABLE QVariant callLisp(const QVariant&);
};

