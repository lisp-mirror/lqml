import QtQuick

Item {
  objectName: "dynamic"

  property Component component
  property Item item

  function createItem(file) {
    // for custom QML items to be loaded on top of REPL app
    if (item != null) {
      item.destroy()
    }
    Engine.clearCache()
    var pre = (Qt.platform.os === "windows") ? "file:/" : "file://"
    component = Qt.createComponent(pre + file)
    if (component.status === Component.Ready) {
      item = component.createObject()
      return item
    }
    return null
  }
}
