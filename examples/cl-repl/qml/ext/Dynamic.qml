import QtQuick 2.15

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
    component = Qt.createComponent("file://" + file)
    if (component.status === Component.Ready) {
      item = component.createObject()
      return item
    }
    return null
  }
}
