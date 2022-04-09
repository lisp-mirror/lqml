import QtQuick 2.15
import QtQuick.Controls 2.15

Dialog {
  anchors.centerIn: parent
  title: "LQML"
  modal: true
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
