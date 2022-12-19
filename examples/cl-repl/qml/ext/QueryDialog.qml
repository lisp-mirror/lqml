import QtQuick 2.15
import QtQuick.Controls 2.15
import "." as Ext

Rectangle {
  id: queryDialog
  objectName: "query_dialog"
  color: "#f0f0f0"
  visible: false

  Column {
    anchors.fill: parent

    Ext.MenuBack {
      id: menuBack
      label: "Query Dialog"
    }

    TextField {
      id: queryInput
      objectName: "query_input"
      width: parent.width
      font.family: "Hack"
      font.pixelSize: 18
      inputMethodHints: Qt.ImhNoAutoUppercase | Qt.ImhNoPredictiveText

      onAccepted: {
        Lisp.call("dialogs:exited")
        Lisp.call("editor:ensure-output-visible")
      }
    }

    Text {
      objectName: "query_text"
      width: parent.width
      height: main.availableHeight() - menuBack.height - queryInput.height
      leftPadding: 8
      rightPadding: 8
      topPadding: 8
      font.pixelSize: 18
    }
  }

  Row {
    anchors.horizontalCenter: parent.horizontalCenter
    y: queryInput.y + queryInput.height + (main.small ? 7 : 10)
    spacing: 20
    visible: queryInput.focus

    // cursor back
    Ext.ArrowButton {
      opacity: 0.15
      text: "\uf137"

      onPressed:      queryInput.cursorPosition--
      onPressAndHold: queryInput.cursorPosition = 0
    }

    // cursor forward
    Ext.ArrowButton {
      opacity: 0.15
      text: "\uf138"

      onPressed:      queryInput.cursorPosition++
      onPressAndHold: queryInput.cursorPosition = queryInput.length
    }
  }
}
