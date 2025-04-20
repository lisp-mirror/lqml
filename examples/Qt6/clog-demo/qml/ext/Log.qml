import QtQuick
import QtQuick.Controls
import "." as Ext

Rectangle {
  color: "lavender"

  Ext.Repl {}

  function log(message) {
    logModel.append({ message: message })
    listView.positionViewAtEnd()
  }

  ListView {
    id: listView
    anchors.fill: parent
    model: ListModel { id: logModel }
    delegate: Text {
      font.pixelSize: 14
      text: message
    }
  }
}
