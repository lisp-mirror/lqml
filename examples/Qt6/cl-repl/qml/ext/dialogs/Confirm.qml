import QtQuick
import QtQuick.Dialogs

MessageDialog {
  title: "LQML"
  buttons: MessageDialog.Save | MessageDialog.Cancel

  property string callback

  onAccepted: Lisp.call(callback, true)
  onRejected: Lisp.call(callback, false)
}
