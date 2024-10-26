import QtQuick
import QtQuick.Controls
import QtQuick.Controls.Basic

Dialog {
  anchors.centerIn: parent
  title: "Confirm"
  font.pixelSize: 18
  modal: true
  standardButtons: Dialog.Save | Dialog.Cancel

  property alias text: message.text
  property string callback

  Text {
    id: message
    width: parent.width // without width word wrap won't work
    wrapMode: Text.Wrap
    font.pixelSize: 18
  }

  onAccepted: Lisp.call(callback, true)
  onRejected: Lisp.call(callback, false)
}
