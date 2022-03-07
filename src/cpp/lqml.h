#pragma once

#undef SLOT

#include <ecl/ecl.h>
#include <QObject>
#include <QByteArray>
#include <QStringList>
#include <QCoreApplication>

class QQuickView;

QT_BEGIN_NAMESPACE

#define EVAL_ERROR_VALUE -1

typedef void (*lisp_ini)(cl_object);

class LQML : public QObject {
  Q_OBJECT
public:
  LQML(int, char* [], QQuickView*);
  ~LQML();

  static bool cl_booted_p;
  static bool cl_shutdown_p;
  static const char version[];
  static QEventLoop* eventLoop;
  static void ini(int, char* []);
  static void eval(const QString&, bool = false);
  static LQML* me;
  static QQuickView* quickView;

  void exec(lisp_ini, const QByteArray& = "nil", const QByteArray& = "qml-user");
  void ignoreIOStreams();
    
  void printVersion() {
    eval("(multiple-value-bind (lqml qt)"
         "    (qml:qversion)"
         "  (format t \"LQML ~A (ECL ~A, Qt ~A)~%~%\" lqml (lisp-implementation-version) qt))");
  }

  Q_INVOKABLE void runOnUiThread(void*);

public Q_SLOTS:
  void exitEventLoop() { eventLoop->exit(); }
};

QT_END_NAMESPACE

