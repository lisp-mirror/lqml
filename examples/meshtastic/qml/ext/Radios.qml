import QtQuick 2.15
import "." as Ext

Rectangle {
  color: "#b3cde3"

  ListView {
    id: view
    anchors.fill: parent
    anchors.margins: 9
    spacing: 9
    delegate: radioDelegate
    model: radios
  }

  ListModel {
    id: radios
    objectName: "radios"

    // hack to define all model key _types_
    ListElement {
      name: ""; hwModel: ""; batteryLevel: 0; current: false
    }

    function addRadio(radio) {
      append(radio)
      if (radio.current) {
        view.currentIndex = view.count - 1
      }
    }

    Component.onCompleted: remove(0) // see hack above
  }

  Component {
    id: radioDelegate

    Rectangle {
      id: delegate
      width: Math.min(265, view.width)
      height: 35
      color: (index === view.currentIndex) ? "firebrick" : "steelblue"
      radius: height / 2

      Rectangle {
        x: 10
        width: 42
        height: 15
        anchors.verticalCenter: parent.verticalCenter
        color: "#f0f0f0"
        radius: height / 2

        Text {
          anchors.centerIn: parent
          font.pixelSize: 12
          font.bold: true
          font.family: fontMono.name
          color: "black"
          text: model.name
        }
      }

      Text {
        x: 58
        anchors.verticalCenter: parent.verticalCenter
        font.pixelSize: 16
        font.family: fontText.name
        color: "white"
        text: model.hwModel
      }

      Ext.BatteryLevel {
        anchors.right: parent.right
        anchors.rightMargin: 14
        level: model.batteryLevel
      }

      MouseArea {
        anchors.fill: parent

        onClicked: {
          view.currentIndex = index
          Lisp.call("radios:change-radio", model.name)
        }
      }
    }
  }
}
