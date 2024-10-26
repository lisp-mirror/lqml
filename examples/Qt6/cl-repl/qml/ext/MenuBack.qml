import QtQuick
import QtQuick.Controls
import QtQuick.Controls.Basic

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
        id: iconBack
        x: 10
        height: backButton.height
        verticalAlignment: Text.AlignVCenter
        font.family: fontAwesome.name
        font.pixelSize: 32
        color: "#007aff"
        text: "\uf104"
      }

      Text {
        x: 30
        height: backButton.height * 1.1 // align correction (different font from above)
        verticalAlignment: Text.AlignVCenter
        font.pixelSize: 20
        font.weight: Font.DemiBold
        color: iconBack.color
        text: "Repl"
        visible: (Qt.platform.os === "ios")
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
