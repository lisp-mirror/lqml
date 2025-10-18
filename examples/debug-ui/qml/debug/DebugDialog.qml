import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15
import "." as Ext

Rectangle {
  id: debugDialog
  objectName: "debug_dialog"
  color: "#f0f0f0"
  visible: false

  ColumnLayout {
    anchors.fill: parent
    spacing: 0

    Ext.MenuBack {
      id: menuBack
      Layout.fillWidth: true
      label: "Debug Dialog"
    }

    TextField {
      id: debugInput
      objectName: "debug_input"
      Layout.fillWidth: true
      font.family: "Hack"
      font.pixelSize: 18
      inputMethodHints: Qt.ImhNoAutoUppercase | Qt.ImhNoPredictiveText
      text: ":q"

      onAccepted: Lisp.call("dialogs:exited")
    }

    Text {
      id: label
      Layout.fillWidth: true
      leftPadding: 8
      rightPadding: 8
      topPadding: 8
      bottomPadding: 8
      font.family: "Hack"
      font.pixelSize: 14
      text: ":r1 etc. restart / :h help / :q quit"
    }

    Rectangle {
      id: line
      Layout.fillWidth: true
      height: 1
      color: "#d0d0d0"
    }

    ListView {
      id: debugText
      objectName: "debug_text"
      Layout.fillWidth: true
      Layout.fillHeight: true
      contentWidth: parent.width * 5
      clip: true
      model: debugModel
      flickableDirection: Flickable.HorizontalAndVerticalFlick

      delegate: Text {
        padding: 8
        textFormat: Text.PlainText
        font.pixelSize: 16
        font.family: "Hack"
        font.bold: model.bold
        text: model.text
        color: model.color
      }
    }

    ListModel {
      id: debugModel
      objectName: "debug_model"

      function appendOutput(data) {
        append(data)
        debugText.positionViewAtEnd()
      }
    }
  }
}
