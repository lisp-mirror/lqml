import QtQuick 2.15
import QtQuick.Dialogs 1.3

Dialog {
  title: "LQML"
  standardButtons: Dialog.Save | Dialog.Cancel

  property alias text: message.text
  property string callback

  Text {
    id: message
    width: parent.width // without width word wrap won't work
    wrapMode: Text.Wrap
  }

  onAccepted: Lisp.call(callback, true)
  onRejected: Lisp.call(callback, false)
}
