import QtQuick

Loader {
  objectName: "ext/Repl.qml"
  source: objectName

  Component.onCompleted: if (width === 0) { anchors.fill = parent }

  function reload() {
    var src = source
    source = ""
    Engine.clearCache()
    source = src
  }
}
