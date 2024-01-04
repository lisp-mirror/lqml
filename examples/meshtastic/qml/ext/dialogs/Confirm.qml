import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Dialogs 1.3

Dialog {
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
