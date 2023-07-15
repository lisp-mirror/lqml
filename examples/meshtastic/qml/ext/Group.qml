import QtQuick 2.15
import QtQuick.Controls 2.15
import "." as Ext

Rectangle {
  id: rect
  color: "#ccebc5"

  Row {
    id: rowModem
    padding: 9
    spacing: 9

    Ext.ComboBox {
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
    anchors.topMargin: rowModem.height
    anchors.fill: parent
    anchors.margins: 9
    spacing: 9
    delegate: groupDelegate
    model: group
    currentIndex: -1
  }

  ListModel {
    id: group
    objectName: "group"

    // hack to define all model key _types_
    ListElement {
      radioName: ""; customName: ""; nodeNum: ""; unread: 0; current: false
    }

    function addPerson(person) {
      append(person)
      if (person.current) {
        view.currentIndex = view.count - 1
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
        x: 10
        width: 42
        height: 15
        anchors.verticalCenter: parent.verticalCenter
        color: "#f0f0f0"
        radius: height / 2

        Text {
          anchors.centerIn: parent
          font.pixelSize: 12
          font.family: fontText.name
          font.weight: Font.DemiBold
          color: "black"
          text: model.radioName
        }
      }

      function selected() {
        view.currentIndex = index
        Lisp.call("lora:change-receiver", model.nodeNum)
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
          readOnly = false
          selectAll()
          forceActiveFocus()
        }

        onEditingFinished: {
          readOnly = true
          if (text === "") text = qsTr("Anonym")
          Lisp.call("group:name-edited", model.radioName, text)
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
        color: "#ff5f57"
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
}

