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

    Ext.MenuBack {
      id: menuBack
      label: "Debug Dialog"

      onPressed: {
        debugInput.text = ":q"
        Lisp.call("dialogs:exited")
      }
    }

    TextField {
      id: debugInput
      objectName: "debug_input"
      Layout.fillWidth: true
      font.family: "Hack"
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
      font.pixelSize: debugInput.font.pixelSize - (main.small ? 4 : 2)
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
        font.pixelSize: debugInput.font.pixelSize - (main.small ? 2 : 0)
        font.family: "Hack"
        font.bold: mBold
        text: mText
        color: mColor
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
