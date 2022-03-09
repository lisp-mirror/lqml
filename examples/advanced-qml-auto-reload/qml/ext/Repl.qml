import QtQuick 2.15
import QtQuick.Controls 2.15

Item {
  id: repl
  z: 2
  anchors.fill: parent

  Row {
    z: 1
    anchors.right: parent.right

    Text {
      text: "REPL"
      anchors.verticalCenter: show.verticalCenter
      visible: !show.checked
    }

    Switch {
      id: show

      onCheckedChanged: container.enabled = checked
    }
  }

  Column {
    id: container
    opacity: 0

    Rectangle {
      width: repl.parent.width
      height: repl.parent.height / 4
      color: "#101010"

      ListView {
        id: replOutput
        objectName: "repl_output"
        anchors.fill: parent
        contentWidth: parent.width * 4
        clip: true
        model: replModel
        flickableDirection: Flickable.HorizontalAndVerticalFlick

        delegate: Column {
          Rectangle {
            width: replOutput.contentWidth
            height: 1
            color: "#707070"
            visible: mLine
          }

          Text {
            x: 2
            padding: 2
            textFormat: Text.PlainText
            font.family: fontHack.name
            font.pixelSize: 16
            font.bold: mBold
            text: mText
            color: mColor
          }
        }
      }

      ListModel {
        id: replModel
        objectName: "repl_model"

        function appendText(data) {
          append(data)
          replOutput.contentX = 0
          replOutput.positionViewAtEnd()
        }
      }
    }

    Row {
      width: repl.parent.width

      TextField {
        id: input
        objectName: "repl_input"
        width: repl.parent.width - 2 * back.width
        font.family: fontHack.name
        font.pixelSize: 16
        font.bold: true
        color: "#c0c0c0"
        inputMethodHints: Qt.ImhNoAutoUppercase | Qt.ImhNoPredictiveText
        focus: show.checked
        palette {
          highlight: "#e0e0e0"
          highlightedText: "#101010"
        }

        background: Rectangle {
          color: "#101010"
          border.width: 2
          border.color: "gray"
        }

        onAccepted: Lisp.call("eval:eval-in-thread", text)
      }

      Button {
        id: back
        objectName: "history_back"
        width: 40
        height: input.height
        focusPolicy: Qt.NoFocus
        font.family: fontIcons.name
        font.pixelSize: 26
        text: "\uf100"

        onClicked: Lisp.call("eval:history-move", "back")
      }

      Rectangle {
        width: 1
        height: input.height
        color: "#101010"
      }

      Button {
        id: forward
        objectName: "history_forward"
        width: back.width
        height: input.height
        focusPolicy: Qt.NoFocus
        font.family: fontIcons.name
        font.pixelSize: 26
        text: "\uf101"

        onClicked: Lisp.call("eval:history-move", "forward")
      }
    }

    Rectangle {
      width: repl.parent.width
      height: 1
      color: "#101010"
    }
  }

  ProgressBar {
    objectName: "progress"
    anchors.top: container.bottom
    width: repl.width
    z: 1
    indeterminate: true
    enabled: visible
    visible: false
  }

  states: [
    State { when: show.checked;  PropertyChanges { target: container; opacity: 0.9; y: 0 }},
    State { when: !show.checked; PropertyChanges { target: container; opacity: 0.0; y: -height }}
  ]

  transitions: [
    Transition { NumberAnimation { properties: "opacity,y"; duration: 250; easing.type: Easing.InCubic }}
  ]
}
