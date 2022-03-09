import QtQuick 2.15

Loader {
  objectName: "~A"
  source: "../" + objectName // TODO: set 'baseUrl' to Engine, so we don't need to fiddle here

  Component.onCompleted: if (width === 0) { anchors.fill = parent }

  function reload() {
    var src = source
    source = ""
    Engine.clearCache()
    source = src
  }
}
