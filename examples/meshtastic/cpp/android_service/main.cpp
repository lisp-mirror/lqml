#include "qtandroidservice_ro.h"
#include "../ble/ble_meshtastic.h"
#include <QAndroidService>

int main(int argc, char* argv[]) {
  QAndroidService app(argc, argv);

  QRemoteObjectHost srcNode(QUrl(QStringLiteral("local:replica")));
  QtAndroidService qtAndroidService;
  srcNode.enableRemoting(&qtAndroidService);

  BLE_ME ble(&qtAndroidService);
  qtAndroidService.ble = &ble;

  return app.exec();
}
