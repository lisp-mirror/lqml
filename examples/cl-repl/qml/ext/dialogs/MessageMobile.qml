import QtQuick 2.15
import QtQuick.Controls 2.15

Dialog {
  anchors.centerIn: parent
  title: "LQML"
  modal: true
  standardButtons: Dialog.Ok

  property alias text: message.text

  Text {
    id: message
    width: parent.width // without width word wrap won't work
    wrapMode: Text.Wrap
  }
}
