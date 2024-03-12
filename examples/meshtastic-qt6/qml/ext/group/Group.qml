import QtQuick
import QtQuick.Controls
import QtQuick.Controls.Basic
import "." as Grp
import "../common/" as Com

Rectangle {
  id: rect
  color: "#ccebc5"

  Row {
    id: rowModem
    padding: 9
    spacing: 9

    Com.ComboBox {
      id: modem
      objectName: "modem"
      width: 160
      font.pixelSize: 16
      font.family: fontText.name

      onActivated: Lisp.call("lora:change-modem-preset", currentText)
    }

    Text {
      height: modem.height
      font.pixelSize: 16
      font.family: fontText.name
      verticalAlignment: Text.AlignVCenter
      text: qsTr("modem preset")
    }
  }

  ListView {
    id: view
    objectName: "group_view"
    anchors.topMargin: rowModem.height
    anchors.bottomMargin: channel.height
    anchors.fill: parent
    anchors.margins: 9
    spacing: 9
    clip: true
    delegate: groupDelegate
    model: group
    currentIndex: -1
  }

  Rectangle {
    id: channel
    anchors.bottom: parent.bottom
    width: parent.width
    height: 28
    color: "#555"

    Text {
      objectName: "channel_name"
      anchors.centerIn: parent
      font.pixelSize: 16
      font.family: fontText.name
      font.weight: Font.DemiBold
      color: rect.color
      text: "cl-app"
    }
  }

  ListModel {
    id: group
    objectName: "group"

    // hack to define all model key _types_
    ListElement {
      radioName: ""; customName: ""; nodeNum: ""; unread: 0; current: false
    }

    function addPerson(person) {
      // insert sorted
      var i = 1; // 0 is broadcast
      var broadcast = (count === 0)
      for (; i < count; i++) {
        if (person.customName < get(i).customName) {
          insert(i, person)
          break
        }
      }
      if (broadcast || (i === count)) {
        append(person)
      }

      if (person.current) {
        view.currentIndex = broadcast ? 0 : i
        view.positionViewAtIndex(view.currentIndex, ListView.Contain)
        rootItem.broadcast = broadcast
      }
    }

    function sortRenamed(name, index) {
      var to = -1
      if (name < get(1).customName) { // 0 is broadcast
        to = 1
      } else if (name >= get(count - 1).customName) {
        to = count - 1
      } else {
        for (var i = 1; i < count; i++) {
          if ((i !== index) && (name < get(i).customName)) {
            to = (index > i) ? i : i - 1
            break
          }
        }
      }
      if (to !== -1) {
        move(index, to, 1)
        view.currentIndex = to
        view.positionViewAtIndex(to, ListView.Contain)
      }
    }

    function radioNames() {
      var names = []
      for (var i = 0; i < count; i++) {
        names.push(get(i).radioName)
      }
      return names
    }

    function setUnread(name, n) {
      for (var i = 0; i < count; i++) {
        if (get(i).radioName === name) {
          setProperty(i, "unread", n)
          break
        }
      }
    }

    Component.onCompleted: remove(0) // see hack above
  }

  Component {
    id: groupDelegate

    Rectangle {
      id: delegate
      width: Math.min(265, view.width)
      height: 35
      color: (index === view.currentIndex) ? "firebrick" : "darkcyan"
      radius: height / 2

      Rectangle {
        id: rectRadio
        x: (index === 0) ? 18 : 10
        width: (index === 0) ? 28 : 42
        height: (index === 0) ? width : 15
        anchors.verticalCenter: parent.verticalCenter
        color: "#f0f0f0"
        radius: height / 2

        Image {
          anchors.centerIn: parent
          width: 20
          height: width
          source: "../../img/broadcast.png"
          visible: (index === 0)
        }

        Text {
          anchors.centerIn: parent
          font.pixelSize: 12
          font.family: fontText.name
          font.weight: Font.DemiBold
          color: "black"
          text: model.radioName
          visible: (index !== 0)
        }
      }

      function selected() {
        view.currentIndex = index
        Lisp.call("lora:change-receiver", model.nodeNum)
        rootItem.broadcast = (index === 0)
      }

      MouseArea {
        id: mouseArea
        anchors.fill: parent

        onClicked: selected()
      }

      // custom name

      TextField {
        id: name
        x: 58
        anchors.verticalCenter: parent.verticalCenter
        leftPadding: 2
        font.pixelSize: 18
        font.family: fontText.name
        font.weight: Font.DemiBold
        color: readOnly ? "white" : "#505050"
        palette.highlight: "darkcyan"
        palette.highlightedText: "white"
        text: (model.customName === "") ? qsTr("Anonym") : model.customName
        readOnly: true

        background: Rectangle {
          y: 4
          width: delegate.width - 1.5 * delegate.height - rectRadio.width
          height: delegate.height - 12
          color: name.readOnly ? "transparent" : "#f0f0f0"
          border.width: 0
        }

        onPressAndHold: {
          if (index !== 0) {
            readOnly = false
            selectAll()
            forceActiveFocus()
            Qt.inputMethod.show() // needed for SailfishOS
          }
        }

        onEditingFinished: {
          if (!readOnly) {
            readOnly = true
            group.setProperty(index, "customName", text)
            Lisp.call("group:name-edited", model.radioName, text)
            if (text === "") text = qsTr("Anonym")
            Qt.callLater(group.sortRenamed, text, index) // 'Qt.callLater': prevent UI thread related crash
          }
        }

        onReleased: if (readOnly) selected()
      }

      // unread messages

      Rectangle {
        anchors.right: parent.right
        anchors.verticalCenter: parent.verticalCenter
        anchors.rightMargin: 8
        width: 22
        height: width
        radius: width / 2
        color: "#ff4040"
        visible: (model.unread > 0)

        Text {
          anchors.fill: parent
          horizontalAlignment: Text.AlignHCenter
          verticalAlignment: Text.AlignVCenter
          font.pixelSize: 12
          font.weight: Font.DemiBold
          font.family: fontText.name
          text: model.unread
          color: "white"
        }
      }
    }
  }

  Grp.Map {
    objectName: "map_view"
    anchors.fill: rect
    visible: false
  }

  Timer {
    interval: 15 * 60 * 1000 // 15 min
    repeat: true
    running: true
    onTriggered: Lisp.call("lora:get-node-config")
  }
}

