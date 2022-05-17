#include "lib.h"
#include "../../src/cpp/ecl_fun_plugin.h" // for ecl_fun(); independent from LQML
#include <QApplication>
#include <QMessageBox>
#include <QtDebug>

QT_BEGIN_NAMESPACE

QObject* ini() {
  // any QObject inherited class will do
  static QObject* cpp = nullptr;
  if (cpp == nullptr) {
    cpp = new CPP;

    // needed for QMessageBox
    static int argc = 0;
    new QApplication(argc, nullptr);
  }
  return cpp;
}

// functions defined Q_INVOKABLE

QVariant CPP::hello(const QVariant& arg) {

  QString msg;
  QDebug debug(&msg);
  debug << arg;

  QMessageBox::information(nullptr, "hello", msg);

  return arg;
}

QVariant CPP::callLisp(const QVariant& arg) {

  return ecl_fun("cl:format", false, "~R", arg);
}

QT_END_NAMESPACE
