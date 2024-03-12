import QtQuick
import QtQuick.Controls
import QtQuick.Dialogs

Dialog {
  anchors.centerIn: parent
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
    }
  }

  onAccepted: Lisp.call(callback, true)
  onRejected: Lisp.call(callback, false)
}
