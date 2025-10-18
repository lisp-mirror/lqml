import QtQuick 2.15
import QtQuick.Controls 2.15

Rectangle {
  id: menuBack
  width: main.width
  height: backButton.height
  color: "#f0f0f0"

  property alias label: label.text

  Button {
    id: backButton
    height: main.small ? 40 : 46
    width: 80

    background: Rectangle {
      Text {
        id: back
        x: 10
        height: backButton.height
        verticalAlignment: Text.AlignVCenter
        color: "#007aff"
        font.pixelSize: 18
        text: qsTr("Back")
      }

      implicitWidth: 90
      color: menuBack.color
    }

    onPressed: Lisp.call("dialogs:exited")
  }

  Text {
    id: label
    anchors.centerIn: parent
    font.pixelSize: 20
    font.weight: Font.DemiBold
  }
}
