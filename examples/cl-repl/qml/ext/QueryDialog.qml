import QtQuick 2.15
import QtQuick.Controls 2.15
import "." as Ext

Popup {
  id: popup
  x: 4
  y: x
  width: parent.width - 2 * x
  height: queryInput.height + text.height + 24
  closePolicy: Popup.NoAutoClose

  onVisibleChanged: main.enabled = !visible

  Rectangle {
    id: queryDialog
    objectName: "query_dialog"
    anchors.fill: parent
    color: "#f0f0f0"

    Column {
      id: column
      width: parent.width

      TextField {
        id: queryInput
        objectName: "query_input"
        width: parent.width
        font.family: "Hack"
        font.pixelSize: 18
        inputMethodHints: Qt.ImhNoAutoUppercase | Qt.ImhNoPredictiveText

        onAccepted: {
          popup.close()
          Lisp.call("dialogs:exited")
          Lisp.call("editor:ensure-output-visible")
        }
      }

      Text {
        id: text
        objectName: "query_text"
        width: parent.width
        padding: 8
        font.pixelSize: 18
      }
    }
  }
}
