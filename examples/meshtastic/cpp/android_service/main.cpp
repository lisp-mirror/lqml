#include "qtandroidservice_ro.h"
#include "../ble/ble_meshtastic.h"

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

  BLE_ME ble(&qtAndroidService);

  return app.exec();
}
