import QtQuick 2.15
import QtQuick.Dialogs 1.3

Dialog {
  title: qsTr("Info")
  standardButtons: Dialog.Ok

  property alias text: message.text

  Text {
    id: message
    width: parent.width
    wrapMode: Text.Wrap
  }
}
