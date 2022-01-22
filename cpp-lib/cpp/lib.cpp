#include "lib.h"
#include <QApplication>
#include <QMessageBox>
#include <QtDebug>

QT_BEGIN_NAMESPACE

QObject* ini() {
  // any QObject inherited class will do
  static QObject* cpp = 0;
  if(!cpp) {
    cpp = new CPP;

    // needed for QMessageBox
    static int argc = 1;
    static char* argv[] = {"cpp"};
    new QApplication(argc, argv);
  }
  return cpp;
}

QVariant CPP::hello(const QVariant& arg) {
  QString msg;
  QDebug debug(&msg);
  debug << arg;

  QMessageBox::information(nullptr, "hello", msg);

  return arg;
}

QT_END_NAMESPACE
