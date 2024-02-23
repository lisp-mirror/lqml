#include "main.h"
#include "lqml.h"
#include "qml_ext.h"
#include <QDir>
#include <QTimer>
#include <QRegularExpression>
#include <QTranslator>
#include <QtDebug>
#include <QtQml/QQmlEngine>
#include <QtQml/QQmlFileSelector>
#include <QtQuick/QQuickView>
#include <QtQuickControls2/QQuickStyle>
#include <iostream>

#ifdef INI_WEBVIEW
  #include <QtWebView>
#endif

#ifdef QT_EXTENSION
  #include "cpp/qt.h"
#endif

#ifdef Q_OS_MACOS
#define ADD_MACOS_BUNDLE_IMPORT_PATH \
  view.engine()->addImportPath(app.applicationDirPath() + QStringLiteral("/../PlugIns"));
#else
#define ADD_MACOS_BUNDLE_IMPORT_PATH
#endif

#if (defined INI_LISP) || (defined BACKGROUND_INI_LISP)
  extern "C" void ini_app(cl_object);
#endif

#ifdef INI_ECL_CONTRIB
  extern "C" {
    void init_lib_DEFLATE(cl_object);
    void init_lib_ECL_CDB(cl_object);
    void init_lib_ECL_HELP(cl_object);
    void init_lib_QL_MINITAR(cl_object);
    void init_lib_SOCKETS(cl_object);
    void init_lib_ECL_CURL(cl_object);
  }
#endif

#ifdef INI_ASDF
  extern "C" void init_lib_ASDF(cl_object);
#endif

int catch_all_qexec() {
  int ret = 0;
  CL_CATCH_ALL_BEGIN(ecl_process_env()) {
    ret = QGuiApplication::exec();
  }
  CL_CATCH_ALL_END;
  return ret;
}

cl_object do_ini_app() {
#ifdef BACKGROUND_INI_LISP
  ecl_init_module(NULL, ini_app);
#endif
  return ECL_NIL;
}

int main(int argc, char* argv[]) {
#if QT_VERSION < 0x060000
  QCoreApplication::setAttribute(Qt::AA_EnableHighDpiScaling);
#endif
#ifdef NO_TEXT_HANDLES
  qputenv("QT_QPA_NO_TEXT_HANDLES", "1");
#endif
  if (QFile::exists("/etc/sailfish-release")) { // SFOS
    QQuickStyle::setStyle("Basic");             // independent from 'qt-runner'
  }
#ifdef INI_WEBVIEW
  QtWebView::initialize();
#endif
  GuiApplication app(argc, argv);
  app.setApplicationName(QFileInfo(app.applicationFilePath()).baseName());
  QStringList arguments(QCoreApplication::arguments());

  Engine engine;
  QQuickView view(&engine, nullptr);
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

  Lisp lisp;
  view.engine()->rootContext()->setContextProperty("Lisp", &lisp);
  view.engine()->rootContext()->setContextProperty("Engine", &engine);

  LQML lqml(argc, argv, &view);
  if (arguments.contains("-v") || arguments.contains("--version")) {
    lqml.printVersion();
    exit(0);
  }

  cl_object l_qml(STRING("QML"));
  si_select_package(l_qml);
  DEFUN ("do-ini-app", do_ini_app, 0)

  QTranslator translator;
  if ((QFile::exists("i18n") && translator.load(QLocale(), QString(), QString(), "i18n"))
      || translator.load(QLocale(), QString(), QString(), ":/i18n")) {
    QCoreApplication::installTranslator(&translator);
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
    view.show();
  }

#if (defined Q_OS_WIN) && (defined DESKTOP_APP)
  lqml.ignoreIOStreams();
#endif

#if (defined Q_OS_ANDROID) || (defined Q_OS_IOS)
  LQML::eval("(qml::%ini-mobile)");
#endif

#ifdef INI_ECL_CONTRIB
  ecl_init_module(NULL, init_lib_DEFLATE);
  ecl_init_module(NULL, init_lib_ECL_CDB);
  ecl_init_module(NULL, init_lib_ECL_HELP);
  ecl_init_module(NULL, init_lib_QL_MINITAR);
  ecl_init_module(NULL, init_lib_SOCKETS);
  ecl_init_module(NULL, init_lib_ECL_CURL);
#endif

#ifdef INI_ASDF
  ecl_init_module(NULL, init_lib_ASDF);
#endif

  // load .eclrc
  if (arguments.contains("-norc")) {
    arguments.removeAll("-norc");
  } else {
#if (!defined Q_OS_ANDROID) && (!defined Q_OS_IOS) && (!defined DESKTOP_APP)
  LQML::eval("(x:when-it (probe-file \"~/.eclrc\")"
             "  (load x:it))");
#endif
  }

#ifdef QT_EXTENSION
  QObject* qt = new QT;
  qt->setParent(&app);
  qt->setObjectName("QT");
#endif

  // load Lisp file
  QStringList files = arguments.filter(".lisp");
  if (!files.isEmpty()) {
    QString file = QDir::fromNativeSeparators(files.first());
    LQML::eval(QString("(load \"%1\")").arg(file), true);
  }

#ifdef INI_LISP
  ecl_init_module(NULL, ini_app);
#endif

#ifdef BACKGROUND_INI_LISP
  LQML::eval("(qml::background-ini)", true); // see 'ini.lisp'
#endif

#ifdef NO_QT_RESTART
  bool qtRestart = false;
#else
  bool qtRestart = true;
#endif

  if (arguments.contains("-slime")
  || (arguments.indexOf(QRegularExpression(".*start-swank.*")) != -1)) {
    arguments.removeAll("-slime");
    qtRestart = true;
  }

  if (qtRestart) {
    LQML::eval("(qml::exec-with-qt-restart)", true);
    return 0;
  }

  return catch_all_qexec();
}

