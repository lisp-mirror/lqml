import QtQuick 2.15
import QtQuick.Controls 2.15
import Lisp 1.0
import "."

Item {
  id: repl
  z: 1
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

      Flickable2 {
        id: flick
        objectName: "flick_output"
        anchors.fill: parent
        contentWidth: output.paintedWidth
        contentHeight: output.paintedHeight

        TextEdit {
          id: output
          objectName: "repl_output"
          width: flick.width
          height: flick.height
          textMargin: 4
          textFormat: TextEdit.RichText
          font.pixelSize: 16
          color: "#c0c0c0"
          readOnly: true

          onCursorRectangleChanged: flick.ensureVisible(cursorRectangle)
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
        font.bold: true
        font.pixelSize: 16
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
