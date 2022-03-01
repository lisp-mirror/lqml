#include <QDir>
#include <QGuiApplication>
#include <QTimer>
#include <QRegularExpression>
#include <QQmlEngine>
#include <QQmlFileSelector>
#include <QQuickView>
#include <iostream>
#include "lqml.h"

#ifdef Q_OS_MACOS
#define ADD_MACOS_BUNDLE_IMPORT_PATH \
  view.engine()->addImportPath(app.applicationDirPath() + QStringLiteral("/../PlugIns"));
#else
#define ADD_MACOS_BUNDLE_IMPORT_PATH
#endif

#ifdef INI_LISP
  extern "C" void ini_app(cl_object);
#endif

#ifdef INI_ECL_CONTRIB
  // for iOS (static lib)
  extern "C" {
    void init_lib_ASDF(cl_object);
    void init_lib_DEFLATE(cl_object);
    void init_lib_ECL_CDB(cl_object);
    void init_lib_ECL_HELP(cl_object);
    void init_lib_QL_MINITAR(cl_object);
    void init_lib_SOCKETS(cl_object);
    void init_lib_ECL_CURL(cl_object);
  }
#endif

int catch_all_qexec() {
  int ret = 0;
  CL_CATCH_ALL_BEGIN(ecl_process_env()) {
    ret = QGuiApplication::exec();
  }
  CL_CATCH_ALL_END;
  return ret;
}

int main(int argc, char* argv[]) {
  QCoreApplication::setAttribute(Qt::AA_EnableHighDpiScaling);
  QGuiApplication app(argc, argv);
  //app.setOrganizationName("MyProject");
  //app.setOrganizationDomain("my.org");
  app.setApplicationName(QFileInfo(app.applicationFilePath()).baseName());
  QStringList arguments(QCoreApplication::arguments());

  QQuickView view;
  ADD_MACOS_BUNDLE_IMPORT_PATH
  view.engine()->addImportPath(QStringLiteral(":/"));
  if (qEnvironmentVariableIntValue("QT_QUICK_CORE_PROFILE")) {
    QSurfaceFormat f = view.format();
    f.setProfile(QSurfaceFormat::CoreProfile);
    f.setVersion(4, 4);
    view.setFormat(f);
  }
  view.connect(view.engine(), &QQmlEngine::quit,
               &app, &QCoreApplication::quit);
  view.connect(&app, &QGuiApplication::lastWindowClosed,
               []() { LQML::eval("(qml:qquit)"); });

  LQML lqml(argc, argv, &view);
  if (arguments.contains("-v") || arguments.contains("--version")) {
    lqml.printVersion();
    std::cout << std::endl;
    exit(0);
  }

  new QQmlFileSelector(view.engine(), &view);
  QString qml("qml/main.qml");
  QUrl url;
  if (QFile::exists(qml)) {         // (1) try local file (development)
    url = QUrl::fromLocalFile(qml);
  } else {
    url = QUrl("qrc:///" + qml);    // (2) use resource file (final app)
  }
  view.setSource(url);
  if (view.status() != QQuickView::Error) {
    view.setResizeMode(QQuickView::SizeRootObjectToView);
#if (defined Q_OS_ANDROID) || (defined Q_OS_IOS)
    view.show();
#else
    QTimer::singleShot(0, &view, &QQuickView::show);
#endif
  }

  // load .eclrc
  if (arguments.contains("-norc")) {
    arguments.removeAll("-norc");
  }
  else {
#if (defined Q_OS_ANDROID) || (defined Q_OS_IOS)
    // mobile: don't hang on startup
    LQML::eval("(x:when-it (probe-file \"~/.eclrc\")"
               "  (ignore-errors (load x:it)))");
#else
    LQML::eval("(x:when-it (probe-file \"~/.eclrc\")"
               "  (load x:it))");
#endif
  }

#ifdef INI_ECL_CONTRIB
  // for iOS (static lib); ASDF is loaded on demand (slow)
  ecl_init_module(NULL, init_lib_DEFLATE);
  ecl_init_module(NULL, init_lib_ECL_CDB);
  ecl_init_module(NULL, init_lib_ECL_HELP);
  ecl_init_module(NULL, init_lib_QL_MINITAR);
  ecl_init_module(NULL, init_lib_SOCKETS);
  ecl_init_module(NULL, init_lib_ECL_CURL);
#endif

#ifdef INI_LISP
  ecl_init_module(NULL, ini_app);
#endif

#ifdef NO_QT_RESTART
  bool slime = false;
#else
  bool slime = true;
#endif

  if (arguments.contains("-slime")
  || (arguments.indexOf(QRegularExpression(".*start-swank.*")) != -1)) {
    arguments.removeAll("-slime");
    slime = true;
  }

  // load Lisp file
  QStringList lisp = arguments.filter(".lisp");
  if (!lisp.isEmpty()) {
    QString file = QDir::fromNativeSeparators(lisp.first());
    LQML::eval(QString("(load \"%1\")").arg(file), true);
  }

  if (slime) {
    LQML::eval("(qml::exec-with-qt-restart)", true);
    return 0;
  }

  return catch_all_qexec();
}

