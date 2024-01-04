import QtQuick 2.15
import QtQuick.Controls 2.15

Dialog {
  anchors.centerIn: parent
  font.pixelSize: 18
  modal: true
  standardButtons: Dialog.Ok | Dialog.Cancel

  property alias text: message.text
  property string callback

  Column {
    width: parent.width
    spacing: 5

    Text {
      id: message
      width: parent.width
      wrapMode: Text.Wrap
      font.pixelSize: 18
    }
  }

  onAccepted: Lisp.call(callback, true)
  onRejected: Lisp.call(callback, false)
}
