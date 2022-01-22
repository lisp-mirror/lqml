#include "lib.h"
#include <QtDebug>

QT_BEGIN_NAMESPACE

QObject* ini() {
  // any QObject inherited class will do
  static QObject* cpp = 0;
  if(!cpp) {
    cpp = new CPP;
  }
  return cpp;
}

// insert here your function implementations

QVariant CPP::hello(const QVariant& arg) {
  QString msg;
  QDebug debug(&msg);
  debug << arg;

  qDebug() << "hello" << arg;

  return arg;
}

QT_END_NAMESPACE
