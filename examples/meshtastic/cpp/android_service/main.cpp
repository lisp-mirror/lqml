#include "qtandroidservice_ro.h"
#include <android/log.h>

#if (QT_VERSION < 0x060000)
  #include <QAndroidService>
#else
  #include <QtCore/private/qandroidextras_p.h>
#endif

static void logMessageHandler(QtMsgType, const QMessageLogContext& context, const QString& msg) {
  // for logging 'qDebug()' output
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

int main(int argc, char* argv[]) {
  QAndroidService app(argc, argv);

  qInstallMessageHandler(logMessageHandler);

  QRemoteObjectHost srcNode(QUrl(QStringLiteral("local:replica")));
  QtAndroidService qtAndroidService;
  srcNode.enableRemoting(&qtAndroidService);

  Connection con(&qtAndroidService);

  return app.exec();
}
