import QtQuick
import QtQuick.Dialogs

Dialog {
  anchors.centerIn: parent
  title: qsTr("Info")
  standardButtons: Dialog.Ok

  property alias text: message.text

  Text {
    id: message
    width: parent.width
    wrapMode: Text.Wrap
  }
}
