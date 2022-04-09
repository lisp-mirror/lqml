import QtQuick 2.15
import QtQuick.Dialogs 1.3

Dialog {
  title: "LQML"
  standardButtons: Dialog.Save | Dialog.Cancel

  property alias text: message.text
  property string callback

  Text {
    id: message
    wrapMode: Text.WordWrap
  }

  onAccepted: Lisp.call(callback, true)
  onRejected: Lisp.call(callback, false)
}
