import QtQuick

Item {
  objectName: "dynamic"

  property Component box:    Qt.createComponent("dynamic/Box.qml")
  property Component box2:   Qt.createComponent("dynamic/Box2.qml")
  property Component player: Qt.createComponent("dynamic/Player.qml")
  property Component fixed:  Qt.createComponent("dynamic/Fixed.qml")

  function createItem(name) {
    switch (name) {
      case "object":  return box.createObject()
      case "object2": return box2.createObject()
      case "player":
      case "player2": return player.createObject()
      case "wall":
      case "goal":    return fixed.createObject()
    }
  }
}
