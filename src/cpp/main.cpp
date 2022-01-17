#include <QDir>
#include <QGuiApplication>
#include <QTimer>
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

int catch_all_qexec() {
  int ret = 0;
  CL_CATCH_ALL_BEGIN(ecl_process_env()) {
    ret = QGuiApplication::exec();
  }
  CL_CATCH_ALL_END;
  return ret;
}

int main(int argc, char* argv[]) {
  QGuiApplication app(argc, argv);
  //app.setOrganizationName("MyProject");
  //app.setOrganizationDomain("my.org");
  app.setApplicationName(QFileInfo(app.applicationFilePath()).baseName());

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
  QStringList arguments(QCoreApplication::arguments());
  if (arguments.contains("-v") || arguments.contains("--version")) {
    lqml.printVersion();
    std::cout << std::endl;
    exit(0);
  }

  new QQmlFileSelector(view.engine(), &view);
  QUrl url("qrc:///qml/main.qml");
  if (!QFile::exists(url.fileName())) {
    url = "qml/main.qml";
  }
  view.setSource(url); 
  if (view.status() == QQuickView::Error) {
    return -1;
  }
  view.setResizeMode(QQuickView::SizeRootObjectToView);
  QTimer::singleShot(0, &view, &QQuickView::show);

  // load .eclrc
  if (arguments.contains("-norc")) {
    arguments.removeAll("-norc");
  }
  else {
    LQML::eval("(x:when-it (probe-file \"~/.eclrc\") (load x:it))");
  }

  bool slime = false;
  if (arguments.contains("-slime")) {
    arguments.removeAll("-slime");
    slime = true;
  }

  // load Lisp file
  if (arguments.length() > 1) {
    QString arg1(QDir::fromNativeSeparators(arguments.at(1)));
    if (arg1.endsWith(".lisp")) {
      LQML::eval(QString("(load \"%1\")").arg(arg1));
    }
  }

  if (slime) {
    LQML::eval("(loop (with-simple-restart (restart-qt-events \"Restart Qt event processing.\") (qexec)))",
               true);
    return 0;
  }

  return catch_all_qexec();
}

