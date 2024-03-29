import QtQuick
import QtQuick.Controls
import QtQuick.Dialogs

Dialog {
  anchors.centerIn: parent
  standardButtons: Dialog.Ok | Dialog.Cancel

  property alias label: label.text
  property alias text: edit.text
  property alias maxLength: edit.maximumLength
  property alias from: spinBox.from
  property alias to: spinBox.to
  property alias value: spinBox.value
  property string callback

  function focus() { edit.forceActiveFocus() }

  Column {
    width: parent.width
    spacing: 5

    Text {
      id: label
      width: parent.width
      wrapMode: Text.Wrap
      visible: (text !== "")
    }

    TextField {
      id: edit
      objectName: "dialog_line_edit"
      width: parent.width
      visible: !spinBox.visible
    }

    SpinBox {
      id: spinBox
      objectName: "dialog_spin_box"
      anchors.horizontalCenter: parent.horizontalCenter
      visible: !!value
    }
  }

  onAccepted: Lisp.call(callback, true)
  onRejected: Lisp.call(callback, false)
}
