import QtQuick 2.15
import QtQuick.Controls 2.15

Rectangle {
  id: menuBack
  width: main.width
  height: backButton.height
  color: "#f0f0f0"

  property string label

  signal pressed()

  Button {
    id: backButton
    height: main.small ? 40 : 46
    width: 80

    background: Rectangle {
      Text {
        id: iconBack
        x: 10
        height: backButton.height
        verticalAlignment: Text.AlignVCenter
        font.family: fontAwesome.name
        font.pixelSize: 32
        color: "#007aff"
        text: "\uf104"
      }
      color: menuBack.color
    }

    onPressed: parent.pressed()
  }

  Text {
    anchors.centerIn: parent
    text: menuBack.label
    font.weight: Font.DemiBold
  }
}
