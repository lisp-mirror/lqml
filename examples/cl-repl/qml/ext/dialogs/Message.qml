import QtQuick 2.15
import QtQuick.Dialogs 1.3

Dialog {
  title: "Title"
  standardButtons: Dialog.Ok

  property alias text: message.text

  Text {
    id: message
    width: parent.width // without width word wrap won't work
    wrapMode: Text.Wrap
  }
}
