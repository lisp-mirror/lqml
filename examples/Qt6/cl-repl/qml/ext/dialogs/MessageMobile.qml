import QtQuick
import QtQuick.Controls
import QtQuick.Controls.Basic

Dialog {
  anchors.centerIn: parent
  title: "Info"
  font.pixelSize: 18
  modal: true
  standardButtons: Dialog.Ok

  property alias text: message.text

  Text {
    id: message
    width: parent.width // without width word wrap won't work
    wrapMode: Text.Wrap
    font.pixelSize: 18
  }
}
