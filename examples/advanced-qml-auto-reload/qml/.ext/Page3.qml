import QtQuick 2.15

Loader {
  objectName: "ext/Page3.qml"
  source: "../" + objectName

  Component.onCompleted: if (width === 0) { anchors.fill = parent }

  function reload() {
    var src = source
    source = ""
    Engine.clearCache()
    source = src
  }
}
