import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Window 2.15

Item {
  id: main
  objectName: "main"
  width: 300
  height: 500

  function availableHeight() {
    var h = Math.round(Qt.inputMethod.keyboardRectangle.y /
                       ((Qt.platform.os === "android") ? Screen.devicePixelRatio : 1))
    return (h === 0) ? main.height : h
  }

  Rectangle {
    anchors.fill: parent
    color: loading.visible ? "#1974d3" : "#e5d8bd"
  }

  ListView {
    id: view
    objectName: "view"
    width: parent.width
    height: availableHeight() - rectEdit.height - 3
    anchors.margins: 3
    spacing: 3
    delegate: messageDelegate
    model: messages
  }

  ListModel {
    id: messages
    objectName: "messages"

    // hack to define all model key _types_
    ListElement {
      text: ""; sender: ""; me: true; timestamp: ""; mid: 0; ackState: 0
    }

    function addMessage(message) {
      append(message)
      view.positionViewAtEnd()
    }

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

    Item {
      id: delegate
      width: Math.max(text.contentWidth, rowSender.width + 4 * text.padding) + 2 * text.padding
      height: text.contentHeight + 2 * text.padding + sender.contentHeight

      Rectangle {
        anchors.fill: parent
        color: model.me ? "#f2f2f2" : "#ffffcc"
        radius: 12
        border.width: 0
        border.color: "#dc1128"

        Row {
          id: rowSender
          padding: text.padding
          spacing: padding

          AnimatedImage {
            id: semaphore
            playing: false
            y: 2
            width: 8
            height: width
            source: "img/semaphore.gif"
            currentFrame: model.ackState
            visible: model.me
          }

          Text {
            id: sender
            font.pixelSize: 10
            font.bold: true
            font.family: fontMono.name
            color: "#8B0000"
            text: model.sender
          }
        }

        Text {
          id: timestamp
          x: delegate.width - contentWidth - text.padding
          y: text.padding
          font.pixelSize: 10
          font.family: fontText.name
          color: "#505050"
          text: model.timestamp
        }

        Text {
          id: text
          y: sender.contentHeight
          width: main.width
          padding: 5
          wrapMode: Text.Wrap
          font.pixelSize: 18
          font.family: fontText.name
          color: "#303030"
          text: model.text
        }
      }
    }
  }

  Rectangle {
    id: rectEdit
    anchors.bottom: parent.bottom
    width: parent.width
    height: edit.paintedHeight + 14
    border.width: 2
    border.color: edit.focus ? "#228ae3" : "#c0c0c0"
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
      source: "img/send.png"
      visible: edit.focus

      MouseArea {
        anchors.fill: parent
        onClicked: {
          edit.focus = Qt.NoFocus
          Lisp.call("radio:send-message", edit.text)
          edit.clear()
        }
      }
    }
  }

  // shown while loading app (may take a while)
  Item {
    visible: loading.visible
    anchors.fill: parent

    AnimatedImage {
      id: loading
      objectName: "loading"
      anchors.centerIn: parent
      source: "img/busy.webp"
      visible: playing
      playing: true
    }

    Text {
      id: iniCount
      anchors.centerIn: parent
      color: "white"
      font.family: fontText.name
      font.pixelSize: 22
    }

    Timer {
      running: loading.playing
      interval: 4500
      repeat: true
      onTriggered: iniCount.text = Number(iniCount.text) + 1
    }
  }

  AnimatedImage {
    objectName: "busy"
    anchors.centerIn: parent
    width: 42
    height: width
    z: 10
    source: "img/busy.gif"
    visible: playing
    playing: false
  }

  FontLoader { id: fontText;  source: "fonts/tahoma.ttf" }
  FontLoader { id: fontMono;  source: "fonts/Hack-Regular.ttf" }
  FontLoader { id: fontMono2; source: "fonts/Hack-Bold.ttf" }
}
