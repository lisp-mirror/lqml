#include "qtandroidservice_ro.h"

#if (QT_VERSION < 0x060000)
  #include <QAndroidService>
#else
  #include <QtCore/private/qandroidextras_p.h>
#endif

int main(int argc, char* argv[]) {
  QAndroidService app(argc, argv);

  QRemoteObjectHost srcNode(QUrl(QStringLiteral("local:replica")));
  QtAndroidService qtAndroidService;
  srcNode.enableRemoting(&qtAndroidService);

  return app.exec();
}
