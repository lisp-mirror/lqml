#include "lqml.h"
#include "ecl_ext.h"
#include <iostream>
#include <QCoreApplication>
#include <QTimer>
#include <QStringList>
#include <QQuickView>
#include <QDebug>

const char LQML::version[] = "23.7.2"; // July 2023

extern "C" void ini_LQML(cl_object);

#ifdef Q_OS_ANDROID

#include <android/log.h>

static void logMessageHandler(QtMsgType, const QMessageLogContext& context, const QString& msg) {
  // for logging on android (see 'adb logcat')
  // examples:
  //   Lisp: (qlog "x: ~A y: ~A" x y)
  //   QML:  console.log("message")
  QString report(msg);
  if (context.file && !QString(context.file).isEmpty()) {
    report += " in file ";
    report += QString(context.file);
    report += " line ";
    report += QString::number(context.line);
  }
  if (context.function && !QString(context.function).isEmpty()) {
    report += " function ";
    report += QString(context.function);
  }
  __android_log_write(ANDROID_LOG_DEBUG, "[LQML]", report.toLocal8Bit().constData());
}

#endif

LQML::LQML(int argc, char* argv[], QQuickView* view) : QObject() {
  me = this;
  quickView = view;
#ifdef Q_OS_ANDROID
  qInstallMessageHandler(logMessageHandler); // see above
#endif
  if (!cl_booted_p) {
    cl_boot(argc, argv);
  }
  iniCLFunctions();
  ecl_init_module(NULL, ini_LQML);
  eval("(in-package :qml-user)");
  eval(QString("(setf qml:*quick-view* (qml:qt-object %1))")
              .arg(reinterpret_cast<quintptr>(view)));
  eval(QString("(setf qml:*engine* (qml:qt-object %1))")
              .arg(reinterpret_cast<quintptr>(view->engine())));
}

LQML::~LQML() {
  if (!LQML::cl_shutdown_p) {
    cl_shutdown();
  }
}

void LQML::ini(int argc, char* argv[]) {
  cl_booted_p = true;
  cl_boot(argc, argv);
}

static cl_object safe_eval(const char* lisp_code) {
  cl_object ret = ECL_NIL;
  CL_CATCH_ALL_BEGIN(ecl_process_env()) {
    ret = si_safe_eval(3,
                       ecl_read_from_cstring(lisp_code),
                       ECL_NIL,
                       ecl_make_fixnum(EVAL_ERROR_VALUE));
  }
  CL_CATCH_ALL_END;
  return ret;
}

static void safe_eval_debug(const char* lisp_code) {
  CL_CATCH_ALL_BEGIN(ecl_process_env()) {
    si_safe_eval(2, ecl_read_from_cstring((char*)lisp_code), ECL_NIL);
  }
  CL_CATCH_ALL_END;
}

void LQML::eval(const QString& lisp_code, bool slime) {
  if (slime) {
    safe_eval_debug(lisp_code.toLatin1().constData());
  } else {
    cl_object ret = safe_eval(lisp_code.toLatin1().constData());
    if ((ecl_t_of(ret) == t_fixnum) && (fix(ret) == EVAL_ERROR_VALUE)) {
      qDebug() << "Error evaluating " << lisp_code;
      exit(-1);
    }
  }
}

void LQML::ignoreIOStreams() {
  // [Windows] print output would cause a gui exe to crash (without console)
  eval("(qml::ignore-io-streams)");
}
    
void LQML::exec(lisp_ini ini, const QByteArray& expression, const QByteArray& package) {
  ecl_init_module(NULL, ini);
  eval(QString("(in-package :%1)").arg(QString(package)));
  eval(expression);
}

void LQML::runOnUiThread(void* function_or_closure) {
  const cl_env_ptr l_env = ecl_process_env();
  CL_CATCH_ALL_BEGIN(l_env) {
    CL_UNWIND_PROTECT_BEGIN(l_env) {
      cl_object l_fun = (cl_object)function_or_closure;
      cl_funcall(1, l_fun);
    }
    CL_UNWIND_PROTECT_EXIT {}
    CL_UNWIND_PROTECT_END;
  }
  CL_CATCH_ALL_END;
}

bool        LQML::cl_booted_p   = false;
bool        LQML::cl_shutdown_p = false;
QEventLoop* LQML::eventLoop     = 0;
LQML*       LQML::me            = nullptr;
QQuickView* LQML::quickView     = nullptr;

