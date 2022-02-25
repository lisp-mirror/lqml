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
  view.connect(view.engine(), &QQmlEngine::quit, &app, &QCoreApplication::quit);

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

#ifdef INI_LISP
  ecl_init_module(NULL, ini_app);
#endif

  bool slime = false;
  if (arguments.contains("-slime")
  || (arguments.indexOf(QRegularExpression(".*start-swank.*")) != -1)) {
    arguments.removeAll("-slime");
    slime = true;
  }

  // load Lisp file
  if (arguments.length() > 1) {
    QString arg1(QDir::fromNativeSeparators(arguments.at(1)));
    if (arg1.endsWith(".lisp")) {
      LQML::eval(QString("(load \"%1\")").arg(arg1), true);
    }
  }

#ifdef SWANK
  slime = true;
#endif

  if (slime) {
    // fallback restart for conditions while processing the Qt event loop
    LQML::eval("(loop (with-simple-restart (restart-qt-events \"Restart Qt event processing.\")"
               "        (qexec)))",
               true);
    return 0;
  }

  return catch_all_qexec();
}

