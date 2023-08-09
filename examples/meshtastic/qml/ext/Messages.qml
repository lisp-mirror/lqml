import QtQuick 2.15
import QtQuick.Controls 2.15

Rectangle {
  id: main
  color: loading.visible ? "#1974d3" : "#e5d8bd"

  ListView {
    id: view
    objectName: "message_view"
    anchors.topMargin: rectFind.height + 4
    anchors.fill: parent
    anchors.bottomMargin: rectEdit.height + 3
    anchors.margins: 5
    delegate: messageDelegate
    model: messages
    clip: true
  }

  ListModel {
    id: messages
    objectName: "messages"

    // hack to define all model key _types_
    ListElement {
      receiver: ""; sender: ""; senderName: ""; timestamp: ""; hour: "";
      text: ""; text2: ""; mid: ""; ackState: 0; me: true; hidden: false
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

    function find(term) {
      for (var i = 0; i < count; i++) {
        var text = get(i).text
        var highlighted = Lisp.call("msg:highlight-term", text, term)
        if (highlighted) {
          if (!get(i).text2) {
            setProperty(i, "text2", text)
          }
          setProperty(i, "text", highlighted)
        }
        setProperty(i, "hidden", !highlighted)
      }
      view.positionViewAtBeginning()
    }

    function clearFind() {
      for (var i = 0; i < count; i++) {
        var text2 = get(i).text2
        if (text2) {
          setProperty(i, "text", text2)
          setProperty(i, "text2", "")
        }
        setProperty(i, "hidden", false)
      }
    }

    Component.onCompleted: remove(0) // see hack above
  }

  Component {
    id: messageDelegate

    Item {
      id: delegate
      width: Math.max(text.paintedWidth, rowSender.width + 4 * text.padding) + 2 * text.padding + 4
      height: model.hidden ? 0 : (text.contentHeight + 2 * text.padding + sender.contentHeight + 8)
      clip: true

      Rectangle {
        anchors.centerIn: parent
        width: parent.width
        height: parent.height - 4
        color: model.me ? "#f2f2f2" : "#ffffcc"
        radius: 12

        MouseArea {
          anchors.fill: parent
          onPressAndHold: Lisp.call("msg:message-press-and-hold", model.text)
        }

        Row {
          id: rowSender
          padding: text.padding
          spacing: padding - 2

          AnimatedImage {
            id: semaphore
            playing: false
            y: 4
            width: 8
            height: width
            source: "../img/semaphore.gif"
            currentFrame: model.ackState
            visible: model.me
          }

          Text {
            id: sender
            font.pixelSize: 12
            font.family: fontText.name
            color: "#8B0000"
            text: model.senderName ? model.senderName : model.sender
          }
        }

        Text {
          id: timestamp
          x: delegate.width - contentWidth - text.padding
          y: text.padding
          font.pixelSize: 12
          font.family: fontText.name
          color: "#505050"
          text: model.hour

          MouseArea {
            anchors.fill: parent
            onClicked: Lisp.call("msg:show-date", model.timestamp)
          }
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
          textFormat: Text.StyledText // for 'paintedWidth' to always work
          text: model.text
        }
      }
    }
  }

  // find text

  TextField {
    id: findText
    objectName: "find_text"
    y: 1
    width: parent.width
    height: visible ? (edit.paintedHeight + 14) : 0
    font.pixelSize: 18
    font.family: fontText.name
    selectionColor: "#228ae3"
    selectedTextColor: "white"
    placeholderText: qsTr("search")
    visible: false

    background: Rectangle {
      id: rectFind
      color: "white"
      border.width: 3
      border.color: findText.focus ? "dodgerblue" : "#c0c0c0"
      radius: 12
    }

    onEditingFinished: Lisp.call("msg:find-text", text)
  }

  // send text

  Rectangle {
    id: rectEdit
    anchors.bottom: parent.bottom
    anchors.bottomMargin: 1
    width: parent.width
    height: edit.paintedHeight + 14
    color: "white"
    border.width: 3
    border.color: edit.focus ? (edit.tooLong ? "#ff5f57" : "dodgerblue") : "#c0c0c0"
    radius: 12

    TextArea {
      id: edit
      objectName: "edit"
      anchors.fill: parent
      textFormat: TextEdit.PlainText
      font.pixelSize: 18
      font.family: fontText.name
      selectionColor: "#228ae3"
      selectedTextColor: "white"
      wrapMode: TextEdit.Wrap
      textMargin: 0
      placeholderText: qsTr("message")

      property bool tooLong: false

      onLengthChanged: if (length > 150) Lisp.call("msg:check-utf8-length", text)
    }

    Image {
      anchors.right: parent.right
      anchors.bottom: parent.top
      anchors.margins: 3
      width: 38
      height: width
      source: "../img/send.png"
      visible: edit.focus && !edit.tooLong

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
