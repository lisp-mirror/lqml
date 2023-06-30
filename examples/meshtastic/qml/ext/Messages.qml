import QtQuick 2.15
import QtQuick.Controls 2.15

Rectangle {
  id: main
  color: loading.visible ? "#1974d3" : "#e5d8bd"

  ListView {
    id: view
    objectName: "message_view"
    anchors.fill: parent
    anchors.bottomMargin: rectEdit.height + 5
    anchors.margins: 5
    spacing: 5
    delegate: messageDelegate
    model: messages
    clip: true
  }

  ListModel {
    id: messages
    objectName: "messages"

    // hack to define all model key _types_
    ListElement {
      receiver: ""; sender: ""; senderName: ""; timestamp: ""; hour: ""; text: ""; mid: ""; ackState: 0; me: true
    }

    function addMessage(message) { append(message) }

    function changeState(state, mid) {
      for (var i = count - 1; i >= 0; i--) {
        if (get(i).mid === mid) {
          setProperty(i, "ackState", state)
          break
        }
      }
    }

    Component.onCompleted: remove(0) // see hack above
  }

  Component {
    id: messageDelegate

    Rectangle {
      id: delegate
      width: Math.max(text.contentWidth, rowSender.width + 4 * text.padding) + 2 * text.padding
      height: text.contentHeight + 2 * text.padding + sender.contentHeight
      color: model.me ? "#f2f2f2" : "#ffffcc"
      radius: 12

      Row {
        id: rowSender
        padding: text.padding
        spacing: padding - 2

        AnimatedImage {
          id: semaphore
          playing: false
          y: 3
          width: 8
          height: width
          source: "../img/semaphore.gif"
          currentFrame: model.ackState
          visible: model.me
        }

        Text {
          id: sender
          font.pixelSize: 11
          font.family: fontText.name
          color: "#8B0000"
          text: model.senderName ? model.senderName : model.sender
        }
      }

      Text {
        id: timestamp
        x: delegate.width - contentWidth - text.padding
        y: text.padding
        font.pixelSize: 11
        font.family: fontText.name
        color: "#505050"
        text: model.hour
      }

      Text {
        id: text
        y: sender.contentHeight
        width: main.width - 10
        padding: 5
        wrapMode: Text.Wrap
        font.pixelSize: 18
        font.family: fontText.name
        color: "#303030"
        text: model.text
      }
    }
  }

  Rectangle {
    id: rectEdit
    anchors.bottom: parent.bottom
    width: parent.width
    height: edit.paintedHeight + 14
    border.width: 3
    border.color: edit.focus ? "dodgerblue" : "#c0c0c0"
    radius: 12

    TextArea {
      id: edit
      anchors.fill: parent
      textFormat: TextEdit.PlainText
      font.pixelSize: 18
      font.family: fontText.name
      selectionColor: "#228ae3"
      selectedTextColor: "white"
      wrapMode: TextEdit.Wrap
      textMargin: 0
      placeholderText: qsTr("message")
    }

    Image {
      anchors.right: parent.right
      anchors.bottom: parent.top
      anchors.margins: 3
      width: 38
      height: width
      source: "../img/send.png"
      visible: edit.focus

      MouseArea {
        anchors.fill: parent
        onClicked: {
          edit.focus = Qt.NoFocus
          Lisp.call("lora:send-message", edit.text)
          edit.clear()
        }
      }
    }
  }
}
