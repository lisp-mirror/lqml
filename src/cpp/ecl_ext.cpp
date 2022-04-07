#include "ecl_ext.h"
#include "marshal.h"
#include "lqml.h"
#include "single_shot.h"
#include <QTimer>
#include <QLibrary>
#include <QLibraryInfo>
#include <QGuiApplication>
#include <QThread>
#include <QFile>
#include <QDir>
#include <QClipboard>
#include <QQuickItem>
#include <QQuickView>
#include <QQmlEngine>
#include <QQmlExpression>
#include <QQmlProperty>

#ifdef Q_OS_ANDROID
  #include <QtAndroid>
#endif

QT_BEGIN_NAMESPACE

void iniCLFunctions() {
  cl_object qml(STRING("QML"));
  if (cl_find_package(qml) == ECL_NIL) {
    cl_make_package(1, qml);
  }
  si_select_package(qml);
  DEFUN ("clipboard-text",      clipboard_text,      0)
  DEFUN ("%ensure-permissions", ensure_permissions2, 1)
  DEFUN ("%js",                 js2,                 2)
  DEFUN ("pixel-ratio",         pixel_ratio,         0)
  DEFUN ("%qapropos",           qapropos2,           3)
  DEFUN ("qchildren",           qchildren,           1)
  DEFUN ("qcopy-file",          qcopy_file,          2)
  DEFUN ("qdirectory",          qdirectory,          1)
  DEFUN ("qescape",             qescape,             1)
  DEFUN ("%qexec",              qexec2,              1)
  DEFUN ("qexit",               qexit,               0)
  DEFUN ("qfind-child",         qfind_child,         2)
  DEFUN ("%qfind-children",     qfind_children2,     3)
  DEFUN ("qfrom-utf8",          qfrom_utf8,          1)
  DEFUN ("%qinvoke-method",     qinvoke_method2,     3)
  DEFUN ("%qload-c++",          qload_cpp,           2)
  DEFUN ("%qlog",               qlog2,               1)
  DEFUN ("qnull",               qnull,               1)
  DEFUN ("%qml-get",            qml_get2,            2)
  DEFUN ("%qml-set",            qml_set2,            3)
  DEFUN ("qobject-name",        qobject_name,        1)
  DEFUN ("%qprocess-events",    qprocess_events2,    1)
  DEFUN ("%qquit",              qquit2,              1)
  DEFUN ("%qrun-on-ui-thread",  qrun_on_ui_thread2,  2)
  DEFUN ("%qget",               qget2,               2)
  DEFUN ("%qset",               qset2,               2)
  DEFUN ("%qsingle-shot",       qsingle_shot2,       2)
  DEFUN ("qtranslate",          qtranslate,          3)
  DEFUN ("qversion",            qversion,            0)
  DEFUN ("qt-object-info",      qt_object_info,      1)
  DEFUN ("%reload",             reload2,             0)
  DEFUN ("root-item",           root_item,           0)
  DEFUN ("set-clipboard-text",  set_clipboard_text,  1)
  DEFUN ("%set-shutdown-p",     set_shutdown_p,      1)

  STATIC_SYMBOL_PKG (s_view_status_changed, "VIEW-STATUS-CHANGED", "QML")
  LQML::quickView->connect(LQML::quickView, &QQuickView::statusChanged,
                           [](QQuickView::Status status) {
                             cl_funcall(2,
                                        s_view_status_changed,
                                        ecl_make_fixnum(static_cast<int>(status)));
                           });
}



// *** utils ***

void error_msg(const char* fun, cl_object l_args) {
  // for error messages in ECL functions defined in C++
  STATIC_SYMBOL_PKG (s_break_on_errors, "*BREAK-ON-ERRORS*", "QML")
  if (cl_symbol_value(s_break_on_errors) != ECL_NIL) {
    STATIC_SYMBOL_PKG (s_break, "%BREAK", "QML") // see "ini.lisp"
    cl_funcall(4,
               s_break,
               STRING("~%[LQML:error] ~A ~{~S~^ ~}~%"),
               STRING(fun),
               l_args);
  } else {
    STATIC_SYMBOL (s_error_output, "*ERROR-OUTPUT*")
    cl_format(4,
              cl_symbol_value(s_error_output),
              STRING("~%[LQML:error] ~A ~{~S~^ ~}~%"),
              STRING(fun),
              l_args);
  }
}



// *** main functions ***

cl_object set_shutdown_p(cl_object l_obj) {
  // for internal use
  LQML::cl_shutdown_p = (l_obj != ECL_NIL);
  ecl_return1(ecl_process_env(), l_obj);
}

cl_object qget2(cl_object l_obj, cl_object l_name) {
  /// args: (qt-object name)
  /// Gets a Qt property. Enumerator values are returned as integer values.
  /// Returns T as second return value for successful calls.
  ///   (qget *quick-view* |width|)
  QObject* qobject = toQObjectPointer(l_obj);
  if (ECL_STRINGP(l_name) && (qobject != nullptr)) {
    const QMetaObject* mo = qobject->metaObject();
    int n = mo->indexOfProperty(toCString(l_name));
    if (n != -1) {
      QMetaProperty mp(mo->property(n));
      QVariant var(mp.read(qobject));
      cl_object l_ret1 = from_qvariant(var);
      ecl_return2(ecl_process_env(), l_ret1, ECL_T);
    }
  }
  ecl_process_env()->nvalues = 1;
  error_msg("QGET", LIST2(l_obj, l_name));
  ecl_return1(ecl_process_env(), ECL_NIL);
}

cl_object qset2(cl_object l_obj, cl_object l_args) {
  /// args: (qt-object name1 value1 &optional name2 value2...)
  /// Sets a Qt property. Enumerators have to be passed as integer values.
  /// Returns T as second return value for successful calls.
  ///   (qset *quick-view* |x| 100 |y| 100)
  QObject* qobject = toQObjectPointer(l_obj);
  if (qobject != nullptr) {
    QQuickItem* item = qobject_cast<QQuickItem*>(qobject);
    if (item != nullptr) {
      QByteArray name(toCString(cl_first(l_args)));
      cl_object l_val = cl_second(l_args);
      // special case for QQuickItem to:
      // * not trigger animations while positioning
      // * provide 'setParent()' for dynamic items, needed for 'objectName'
      if (QByteArray("x y parent").indexOf(name) != -1) {
        if (name == "x") {
          item->setX(toReal(l_val));
        } else if (name == "y") {
          item->setY(toReal(l_val));
        } else {
          QObject* o = toQObjectPointer(l_val);
          QQuickItem* parent = qobject_cast<QQuickItem*>(o);
          if (parent != nullptr) {
            // need to set both here
            item->setParent(parent);
            item->setParentItem(parent);
          }
        }
        ecl_return2(ecl_process_env(), l_args, ECL_T);
      }
    }
    const QMetaObject* mo = qobject->metaObject();
    for (cl_object l_do = l_args; l_do != ECL_NIL; l_do = cl_cddr(l_do)) {
      cl_object l_name = cl_first(l_do);
      cl_object l_val = cl_second(l_do);
      int n = mo->indexOfProperty(toCString(l_name));
      if (n == -1) {
        goto fail;
      }
      QMetaProperty mp(mo->property(n));
      QVariant var;
      if (mp.isEnumType()) {
        var = toInt(l_val);
      } else {
#if QT_VERSION < 0x060000
        var = toQVariant(l_val, mp.type());
#else
        var = toQVariant(l_val, mp.typeId());
#endif
      }
      if (!mp.write(qobject, var)) {
        goto fail;
      }
    }
    ecl_return2(ecl_process_env(), l_args, ECL_T);
  }
fail:
  error_msg("QSET", LIST2(l_obj, l_args));
  ecl_return1(ecl_process_env(), ECL_NIL);
}

cl_object qfind_child(cl_object l_obj, cl_object l_name) {
  /// args: (qt-object name)
  /// Calls QObject::findChild<QObject*>().
  ecl_process_env()->nvalues = 1;
  QString name(toQString(l_name));
  if (!name.isEmpty()) {
    QObject* qobject = toQObjectPointer(l_obj);
    if (qobject != nullptr) {
      QObject* obj = qobject->findChild<QObject*>(name);
      if (obj != nullptr) {
        cl_object l_ret = from_qobject_pointer(obj);
        return l_ret;
      }
    }
  }
  error_msg("QFIND-CHILD", LIST2(l_obj, l_name));
  return ECL_NIL;
}

cl_object qfind_children2(cl_object l_obj, cl_object l_name, cl_object l_class) {
  // for internal use
  ecl_process_env()->nvalues = 1;
  QString objectName(toQString(l_name));
  QByteArray className(toCString(l_class));
  QObject* qobject = toQObjectPointer(l_obj);
  if (qobject != nullptr) {
    QObjectList children = qobject->findChildren<QObject*>(objectName);
    cl_object l_children = ECL_NIL;
    for (QObject* child : qAsConst(children)) {
      QByteArray className2(child->metaObject()->className());
      if (className.isEmpty() || (className == className2)) {
        l_children = CONS(from_qobject_pointer(child),
                          l_children);
      }
    }
    l_children = cl_nreverse(l_children);
    return l_children;
  }
  error_msg("QFIND-CHILDREN", LIST3(l_obj, l_name, l_class));
  return ECL_NIL;
}

cl_object qchildren(cl_object l_item) {
  /// args: (item/name)
  /// Like QML function children().
  ecl_process_env()->nvalues = 1;
  QObject* qobject = toQObjectPointer(l_item);
  QQuickItem* item = qobject_cast<QQuickItem*>(qobject); // type check
  if (item != nullptr) {
    QList<QQuickItem*> children = item->childItems();
    cl_object l_children = ECL_NIL;
    for (QQuickItem* child : qAsConst(children)) {
      l_children = CONS(from_qobject_pointer(child),
                        l_children);
    }
    l_children = cl_nreverse(l_children);
    return l_children;
  }
  error_msg("QCHILDREN", LIST1(l_item));
  return ECL_NIL;
}

cl_object qload_cpp(cl_object l_lib_name, cl_object l_unload) { /// qload-c++
  /// args: (library-name &optional unload)
  /// Loads a custom Qt/C++ plugin (see 'cpp-lib' in sources). The LIBRARY-NAME
  /// has to be passed as path to the plugin, without file ending. This offers
  /// a simple way to extend your application with your own Qt/C++ functions.
  /// The plugin will be reloaded (if supported by the OS) every time you call
  /// this function. If the UNLOAD argument is not NIL, the plugin will be
  /// unloaded (if supported by the OS).
  /// N.B: This works only for Qt functions with the following signature:
  /// "QVariant foo(QVariant, ...)" ; max 10 QVariant arguments
  /// Since a QVariant can also be of type QVariantList, this is a perfect fit
  /// for (nested) Lisp lists.
  ///   (defparameter *c++* (qload-c++ "my-lib"))
  ///   (qapropos nil *c++*)                      ; documentation
  ///   (define-qt-wrappers *c++*)                ; Lisp wrapper functions
  static QHash<QString, QLibrary*> libraries;
  QString libName = toQString(l_lib_name);
  bool unload = (l_unload != ECL_NIL);
  if (!libName.isEmpty()) {
    if (!libName.contains('/')) {
      libName.prepend("./");
    }
    QLibrary* lib = libraries.value(libName, 0);
    if (lib) {
      if (lib->isLoaded()) {
        lib->unload(); // for both unload/reload
        if (!unload) {
          cl_sleep(ecl_make_singlefloat(0.5));
        }
      }
    }
    if (unload) {
      ecl_process_env()->nvalues = 1;
      if (lib) {
        delete lib;
        libraries.remove(libName);
        return l_lib_name;
      }
      return ECL_NIL;
    }
    if (!lib) {
      lib = new QLibrary(libName);
      libraries[libName] = lib;
    }
    typedef QObject* (*Ini)();
    Ini ini = (Ini)lib->resolve("ini");
    if (ini) {
      QObject* main = ini();
      if (main) {
        cl_object l_ret = from_qobject_pointer(main);
        ecl_return1(ecl_process_env(), l_ret);
      }
    }
  }
  error_msg("QLOAD-C++", LIST2(l_lib_name, l_unload));
  ecl_return1(ecl_process_env(), ECL_NIL);
}



// *** convenience functions ***

cl_object qtranslate(cl_object l_con, cl_object l_src, cl_object l_n) {
  // called by QML:TR
  QByteArray context(toQString(l_con).toUtf8());
  QByteArray source(toQString(l_src).toUtf8());
  int n = toInt(l_n);
  cl_object l_ret;
  if (n == -1) {
    l_ret = from_qstring(QCoreApplication::translate(context, source));
  } else {
    l_ret = from_qstring(QCoreApplication::translate(context, source, 0, n));
  }
  ecl_return1(ecl_process_env(), l_ret);
}

cl_object qfrom_utf8(cl_object l_ba) {
  /// args: (byte-array)
  /// Returns the BYTE-ARRAY (vector of octets) converted using
  /// QString::fromUtf8().
  cl_object l_ret = from_qstring(QString::fromUtf8(toQByteArray(l_ba)));
  ecl_return1(ecl_process_env(), l_ret);
}

cl_object qescape(cl_object l_str) {
  /// args: (string)
  /// Calls QString::toHtmlEscaped().
  cl_object l_ret = from_qstring(toQString(l_str).toHtmlEscaped());
  ecl_return1(ecl_process_env(), l_ret);
}

cl_object qprocess_events2(cl_object l_exclude_user_input) {
  /// args: (&optional exclude-user-input)
  /// Calls QCoreApplication::processEvents(). Pass T to exclude user input
  /// events during event processing.
  if (l_exclude_user_input != ECL_NIL) {
    QCoreApplication::processEvents(QEventLoop::ExcludeUserInputEvents);
  } else {
    QCoreApplication::processEvents();
  }
  ecl_return1(ecl_process_env(), ECL_T);
}

cl_object qexec2(cl_object l_milliseconds) {
  /// args: (&optional milliseconds)
  /// Calls QCoreApplication::exec(). Optionally pass the time in milliseconds
  /// after which QEventLoop::exit() will be called. See also QSLEEP.
  ecl_process_env()->nvalues = 1;
  if (l_milliseconds != ECL_NIL) {
    static QTimer* timer = 0;
    if (!timer) {
      timer = new QTimer;
      LQML::eventLoop = new QEventLoop;
      timer->setSingleShot(true);
      QObject::connect(timer, &QTimer::timeout, LQML::me, &LQML::exitEventLoop);
    }
    timer->start(toInt(l_milliseconds));
    LQML::eventLoop->exec();
    return l_milliseconds;
  }
  QCoreApplication::exec();
  return ECL_T;
}

cl_object qexit() {
  /// args: ()
  /// Calls QEventLoop::exit(), in order to exit event processing after a call
  /// QEXEC with a timeout. Returns T if the event loop has effectively been
  /// exited.
  ecl_process_env()->nvalues = 1;
  if (LQML::eventLoop) {
    if (LQML::eventLoop->isRunning()) {
      LQML::eventLoop->exit();
      return ECL_T;
    }
  }
  return ECL_NIL;
}

cl_object qsingle_shot2(cl_object l_msec, cl_object l_fun) {
  /// args: (milliseconds function)
  /// A single shot timer similar to QTimer::singleShot().
  ///   (qsingle-shot 1000 'one-second-later)
  ecl_process_env()->nvalues = 1;
  if (l_fun != ECL_NIL) {
    new SingleShot(toInt(l_msec), l_fun); // see 'deleteLater()' in sources
    return l_msec;
  }
  error_msg("QSINGLE-SHOT", LIST2(l_msec, l_fun));
  return ECL_NIL;
}

cl_object qversion() {
  /// args: ()
  /// Returns the LQML version number as 'year.month.counter'. The second
  /// return value is the Qt version as returned by QLibraryInfo::version().
  cl_object l_ret1 = from_cstring(LQML::version);
  cl_object l_ret2 = from_qstring(QLibraryInfo::version().toString());
  ecl_return2(ecl_process_env(), l_ret1, l_ret2);
}

cl_object qrun_on_ui_thread2(cl_object l_function_or_closure, cl_object l_blocking) {
  // for internal use, you should never need to call it explicitely
  ecl_process_env()->nvalues = 1;
  if (l_function_or_closure != ECL_NIL) {
    if (QThread::currentThread() == qGuiApp->thread()) {
      // direct call
      LQML::me->runOnUiThread(l_function_or_closure);
      return ECL_T;
    } else {
      // queued call in main event loop (GUI thread)
      QMetaObject::invokeMethod(LQML::me,
                                "runOnUiThread",
                                (l_blocking != ECL_NIL) ? Qt::BlockingQueuedConnection : Qt::QueuedConnection,
                                Q_ARG(void*, l_function_or_closure)); 
      return ECL_T;
    }
  }
  error_msg("QRUN-ON-UI-THREAD", LIST1(l_function_or_closure));
  return ECL_NIL;
}

cl_object qlog2(cl_object l_msg) {
  // called by QML:QLOG
  // for android logging only; see also 'lqml.cpp::logMessageHandler'
  qDebug() << toQString(l_msg);
  ecl_return1(ecl_process_env(), ECL_NIL);
}

cl_object qnull(cl_object l_arg) {
  /// args: (qt-object)
  /// Only useful if used with UNLESS, in order to check for a valid pointer.
  /// Returns T if the argument is not of type QT-OBJECT.
  ///   (unless (qnull ui:*item*)
  ///     ...)
  QObject* qobject = toQObjectPointer(l_arg);
  ecl_return1(ecl_process_env(), (qobject == nullptr) ? ECL_T : ECL_NIL);
}

cl_object qinvoke_method2(cl_object l_obj, cl_object l_name, cl_object l_args) {
  // for internal use: this is used to call user defined JS functions, and to
  // call user defined Qt/C++ plugin functions.
  // Max. 10 arguments of type T, NIL, INTEGER, FLOAT, STRING, VECTOR of
  // octets, (nested) LIST of mentioned arguments. On Qt side, only QVariant
  // arguments are allowed.
  // N.B. does not support default arguments, if used to call JS functions
  ecl_process_env()->nvalues = 1;
  const int MAX = 10;
  QVariant arg[MAX];
  QGenericArgument genA[MAX];
  const char* v = "QVariant";
  int i = 0;
  for (cl_object l_do_list = l_args; l_do_list != ECL_NIL; l_do_list = cl_cdr(l_do_list), i++) {
    cl_object l_el = cl_car(l_do_list);
    arg[i] = toQVariant(l_el);
    genA[i] = QGenericArgument(v, &arg[i]);
  }
  QGenericArgument null;
  for (; i < MAX; i++) {
    genA[i] = null;
  }
  QObject* qobject = toQObjectPointer(l_obj);
  QByteArray name(toCString(l_name));
  if ((qobject != nullptr) && !name.isEmpty()) {
    QVariant ret;
    QGenericReturnArgument genR(v, &ret);
    QMetaObject::invokeMethod(qobject, name, genR,
                              genA[0], genA[1], genA[2], genA[3], genA[4], genA[5], genA[6], genA[7], genA[8], genA[9]);
    cl_object l_ret = from_qvariant(ret);
    return l_ret;
  }
  error_msg("QJS", LIST3(l_obj, l_name, l_args));
  return ECL_NIL;
}

cl_object js2(cl_object l_item, cl_object l_str) {
  // called by function QML:JS
  ecl_process_env()->nvalues = 1;
  QObject* qobject = toQObjectPointer(l_item);
  if (qobject != nullptr) {
    QQmlExpression exp(LQML::quickView->rootContext(), qobject, toQString(l_str));
    cl_object l_ret = from_qvariant(exp.evaluate());
    return l_ret;
  }
  error_msg("JS", LIST2(l_item, l_str));
  return ECL_NIL;
}

cl_object qml_get2(cl_object l_item, cl_object l_name) {
  // called by QML:QML-GET
  QObject* qobject = toQObjectPointer(l_item);
  QByteArray name = toCString(l_name);
  if ((qobject != nullptr) && !name.isEmpty()) {
    QQmlProperty property(qobject, name);
    if (property.isValid()) {
      cl_object l_val = from_qvariant(property.read());
      ecl_return2(ecl_process_env(), l_val, ECL_T);
    }
  }
  error_msg("QML-GET", LIST2(l_item, l_name));
  ecl_return1(ecl_process_env(), ECL_NIL);
}

cl_object qml_set2(cl_object l_item, cl_object l_name, cl_object l_value) {
  // called by QML:QML-SET
  ecl_process_env()->nvalues = 1;
  QObject* qobject = toQObjectPointer(l_item);
  QByteArray name = toCString(l_name);
  if ((qobject != nullptr) && !name.isEmpty()) {
    QQmlProperty property(qobject, name);
    if (property.isValid()) {
      cl_object l_ret = property.write(toQVariant(l_value, property.propertyType()))
                        ? ECL_T : ECL_NIL;
      return l_ret;
    }
  }
  error_msg("QML-SET", LIST3(l_item, l_name, l_value));
  return ECL_NIL;
}

cl_object qobject_name(cl_object l_obj) {
  /// args: (qt-object)
  /// Returns the QObject::objectName() of passed QT-OBJECT.
  ecl_process_env()->nvalues = 1;
  QObject* qobject = toQObjectPointer(l_obj);
  if (qobject != nullptr) {
    cl_object l_ret = from_qstring(qobject->objectName());
    return l_ret;
  }
  error_msg("QOBJECT-NAME", LIST1(l_obj));
  return ECL_NIL;
}

cl_object qt_object_info(cl_object l_obj) {
  // for internal use
  QString className("?");
  QString objectName("");
  quintptr address = 0;
  QObject* qobject = toQObjectPointer(l_obj);
  if (qobject != nullptr) {
    className = qobject->metaObject()->className();
    int i = -1;
    if ((i = className.indexOf('_')) != -1) {
      className.truncate(i);
    }
    objectName = qobject->objectName();
    address = reinterpret_cast<quintptr>(qobject);
  }
  cl_object l_class = from_qstring(className);
  cl_object l_name = from_qstring(objectName);
  cl_object l_addr = ecl_make_unsigned_integer(address);
  ecl_return3(ecl_process_env(), l_class, l_name, l_addr);
}

cl_object root_item() {
  /// args: ()
  /// Returns the root item of the QQuickView.
  ecl_process_env()->nvalues = 1;
  cl_object l_ret = from_qobject_pointer(LQML::quickView->rootObject());
  ecl_return1(ecl_process_env(), l_ret);
}

cl_object qquit2(cl_object l_status) {
  // called by QML:QQUIT
  int s = toInt(l_status);
  qGuiApp->quit();
  cl_shutdown();
  LQML::cl_shutdown_p = true;
  if (s < 0) {
    abort();
  } else {
    exit(s);
  }
  return ECL_NIL;
}

cl_object pixel_ratio() {
  /// args: ()
  /// Returns the effective device pixel ratio.
  cl_object l_ret = ecl_make_doublefloat(LQML::quickView->effectiveDevicePixelRatio());
  ecl_return1(ecl_process_env(), l_ret);
}

cl_object reload2() {
  // called by QML:RELOAD
  LQML::quickView->engine()->clearComponentCache();
  QUrl source(LQML::quickView->source());
  LQML::quickView->setSource(source);
  cl_object l_ret = from_qstring(source.toString());
  ecl_return1(ecl_process_env(), l_ret);
}

cl_object clipboard_text() {
  /// args: ()
  /// Calls QGuiApplication::clipboard()->text().
  cl_object l_text = from_qstring(QGuiApplication::clipboard()->text());
  ecl_return1(ecl_process_env(), l_text);
}

cl_object set_clipboard_text(cl_object l_text) {
  /// args: (text)
  /// Calls QGuiApplication::clipboard()->setText().
  QGuiApplication::clipboard()->setText(toQString(l_text));
  ecl_return1(ecl_process_env(), l_text);
}

cl_object qcopy_file(cl_object l_from, cl_object l_to) {
  /// args: (from to)
  /// Convenience function for android, for e.g. copying files from 'assets:/',
  /// which can't be accessed directly from Lisp.
  ///   (qcopy-file "assets:/lib/asdf.fas" "asdf.fas")
  bool ok = QFile::copy(toQString(l_from), toQString(l_to));
  ecl_return1(ecl_process_env(), ok ? ECL_T : ECL_NIL);
}

cl_object qdirectory(cl_object l_dir) {
  /// args: (path)
  /// Convenience function for android, which works also with 'assets:/'
  /// paths.
  ///   (qdirectory "assets:/lib")
  QDir dir(toQString(l_dir));
  QFileInfoList infos(dir.entryInfoList(QDir::Dirs | QDir::Files | QDir::NoDotAndDotDot));
  cl_object l_files = ECL_NIL;
  for (QFileInfo info : qAsConst(infos)) {
    QString path(info.absoluteFilePath());
    if (info.isDir()) {
      path.append("/");
    }
    l_files = CONS(from_qstring(path), l_files);
  }
  l_files = cl_nreverse(l_files);
  ecl_return1(ecl_process_env(), l_files);
}

cl_object ensure_permissions2(cl_object l_permissions) {
  /// args: (&rest permissions)
  /// Android only; requests the passed permissions. Returns the list of
  /// granted permissions. If the permission name starts with
  /// 'android.permission.', a Lisp symbol of the name only can be passed,
  /// otherwise the full identifier must be passed.
  ///   (ensure-permissions :access-fine-location)
  ///   (ensure-permissions "com.android.alarm.permission.SET_ALARM")
  cl_object l_ret = ECL_T;
#if (defined Q_OS_ANDROID) && (QT_VERSION > 0x050A00) // 5.10
  typedef QPair<QString, int> StringInt;
  QList<StringInt> permissions;
  if (ECL_LISTP(l_permissions)) {
    int i = 0;
    for (cl_object l_do_list = l_permissions; l_do_list != ECL_NIL; l_do_list = cl_cdr(l_do_list), i++) {
      cl_object l_el = cl_car(l_do_list);
      if (cl_keywordp(l_el) == ECL_T) {
        QString name(toQString(cl_symbol_name(l_el)));
        permissions << StringInt(name.replace(QChar('-'), QChar('_'))
                                     .prepend("android.permission."),
                                 i);
      } else {
        permissions << StringInt(toQString(l_el), i);
      }
    }
  }
  cl_object l_granted = ECL_NIL;
  QList<StringInt> deniedSI;
  QStringList denied;
  for (StringInt p : qAsConst(permissions)) {
    if (QtAndroid::checkPermission(p.first) == QtAndroid::PermissionResult::Granted) {
      l_granted = CONS(cl_nth(ecl_make_fixnum(p.second), l_permissions),
                       l_granted);
    } else {
      deniedSI << p;
      denied << p.first;
    }
  }
  if (!denied.isEmpty()) {
    QEventLoop loop; // custom sync because requestPermissionsSync() may hang
    QtAndroid::requestPermissions(denied, [&](const QtAndroid::PermissionResultMap& res) {
      for (StringInt p : qAsConst(deniedSI)) {
        if (res[p.first] == QtAndroid::PermissionResult::Granted) {
          l_granted = CONS(cl_nth(ecl_make_fixnum(p.second), l_permissions),
                           l_granted);
        }
      }
      loop.exit();
    });
    loop.exec(QEventLoop::ExcludeUserInputEvents);
  }
  l_ret = cl_nreverse(l_granted);
#endif
  ecl_return1(ecl_process_env(), l_ret);
}



// *** meta info ***

static QByteArrayList metaInfo(const QByteArray& type,
                               const QByteArray& qclass,
                               const QByteArray& search,
                               const QMetaObject* mo,
                               bool no_offset = false) {
  QByteArrayList info;
  if ("methods" == type) {
    for (int i = mo->methodOffset(); i < mo->methodCount(); i++) {
      QMetaMethod mm(mo->method(i));
      if (mm.methodType() == QMetaMethod::Method) {
        QString sig(mm.methodSignature());
        QString ret(mm.typeName());
        if (ret.isEmpty()) {
          ret = "void";
        }
        ret.append(" ");
        if (!sig.startsWith("_q_")) {
          QString name(ret + sig);
          QByteArray rm('(' + qclass + '*');
          if (mm.parameterNames().size() > 1) {
            rm.append(',');
          }
          name.replace(rm, "(");
          if (name.contains(search, Qt::CaseInsensitive)) {
            info << name.toLatin1();
          }
        }
      }
    }
  } else if ("properties" == type) {
    // 'no_offset' is for properties only (QML)
    for (int i = (no_offset ? 0 : mo->propertyOffset()); i < mo->propertyCount(); i++) {
      QMetaProperty mp(mo->property(i));
      QString name = QString("%1 %2%3").arg(mp.typeName())
                                       .arg(mp.name())
                                       .arg(mp.isWritable() ? "" : " const");
      if (name.contains(search, Qt::CaseInsensitive)) {
        info << name.toLatin1();
      }
    }
  } else {
    int _type = ("signals" == type) ? QMetaMethod::Signal : QMetaMethod::Slot;
    for (int i = mo->methodOffset(); i < mo->methodCount(); i++) {
      QMetaMethod mm(mo->method(i));
      if (mm.methodType() == _type) {
        QString ret(mm.typeName());
        if (ret.isEmpty()) {
          ret = "void";
        }
        QString sig(mm.methodSignature());
        if (!sig.startsWith("_q_")) {
          QString name(QString("%1 %2").arg(ret).arg(sig));
          if (name.contains(search, Qt::CaseInsensitive)) {
            info << name.toLatin1();
          }
        }
      }
    }
  }
  return info;
}

static bool metaInfoLessThan(const QByteArray& s1, const QByteArray& s2) {
  if (s1.contains('(')) {
    return s1.mid(1 + s1.lastIndexOf(' ', s1.indexOf('('))) <
           s2.mid(1 + s2.lastIndexOf(' ', s2.indexOf('(')));
  }
  return s1.mid(1 + s1.indexOf(' ')) <
         s2.mid(1 + s2.indexOf(' '));
}

static cl_object collectInfo(const QByteArray& type,
                             const QByteArray& qclass,
                             const QByteArray& qsearch,
                             bool* found,
                             const QMetaObject* mo,
                             bool no_offset = false) {
  cl_object l_info = ECL_NIL;
  QByteArrayList info = metaInfo(type, qclass, qsearch, mo, no_offset);
  std::sort(info.begin(), info.end(), metaInfoLessThan);
  if (info.size()) {
    *found = true;
    for (QByteArray i : qAsConst(info)) {
      l_info = CONS(STRING_COPY(i.constData()), l_info);
    }
  }
  l_info = cl_nreverse(l_info);
  return l_info;
}

cl_object qapropos2(cl_object l_search, cl_object l_obj, cl_object l_no_offset) {
  // called by QML:QAPROPOS
  ecl_process_env()->nvalues = 1;  
  QByteArray search;
  if (ECL_STRINGP(l_search)) {
    search = toCString(l_search);
  }
  bool no_offset = (l_no_offset != ECL_NIL); // for QML (all instance properties)
  const QMetaObject* mo = 0;
  QObject* obj = toQObjectPointer(l_obj);
  if (obj != nullptr) {
    mo = obj->metaObject();
    cl_object l_docs = ECL_NIL;
    do {
      bool found = false;
      const QMetaObject* super = mo->superClass();
      QString superName;
      if (super != nullptr) {
        superName = QString(" : %1").arg(super->className());
      }
      QByteArray _class = (QString(mo->className()) + superName).toLatin1();
      cl_object l_doc_pro = ECL_NIL;
      cl_object l_doc_slo = ECL_NIL;
      cl_object l_doc_sig = ECL_NIL;
      l_doc_pro = collectInfo("properties", _class, search, &found, mo, no_offset);
      cl_object l_doc_met = collectInfo("methods", _class, search, &found, mo);
      l_doc_slo = collectInfo("slots", _class, search, &found, mo);
      l_doc_sig = collectInfo("signals", _class, search, &found, mo);
      if (found) {
        cl_object l_doc = ECL_NIL;
        if (l_doc_pro != ECL_NIL) {
          l_doc = CONS(CONS(STRING("Properties:"), l_doc_pro), l_doc);
        }
        if (l_doc_met != ECL_NIL) {
          l_doc = CONS(CONS(STRING("Methods:"), l_doc_met), l_doc);
        }
        if (l_doc_slo != ECL_NIL) {
          l_doc = CONS(CONS(STRING("Slots:"), l_doc_slo), l_doc);
        }
        if (l_doc_sig != ECL_NIL) {
          l_doc = CONS(CONS(STRING("Signals:"), l_doc_sig), l_doc);
        }
        l_doc = cl_nreverse(l_doc);
        if (l_doc != ECL_NIL) {
          l_docs = CONS(CONS(STRING_COPY(_class.data()), l_doc), l_docs);
        }
      }
    } while ((mo = mo->superClass()));
    cl_object l_ret = cl_nreverse(l_docs);
    return l_ret;
  }
  error_msg("QAPROPOS", LIST3(l_search, l_obj, l_no_offset));
  return ECL_NIL;
}

QT_END_NAMESPACE
