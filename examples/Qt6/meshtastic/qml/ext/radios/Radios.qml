import QtQuick
import QtQuick.Controls
import QtQuick.Controls.Basic
import "." as Rad
import "../common/" as Com

Rectangle {
  id: rect
  color: "#b3cde3"

  Row {
    id: rowRegion
    padding: 9
    spacing: 9

    Com.ComboBox {
      id: region
      objectName: "region"
      width: 110
      font.pixelSize: 16
      font.family: fontText.name

      onActivated: Lisp.call("lora:change-region", currentIndex ? currentText : "")
    }

    Text {
      height: region.height
      font.pixelSize: 16
      font.family: fontText.name
      verticalAlignment: Text.AlignVCenter
      text: qsTr("region")
    }
  }

  ListView {
    id: view
    anchors.topMargin: rowRegion.height
    anchors.fill: parent
    anchors.margins: 9
    spacing: 9
    clip: true
    delegate: radioDelegate
    model: radios
  }

  ListModel {
    id: radios
    objectName: "radios"

    // hack to define all model key _types_
    ListElement {
      name: ""; ini: false; hwModel: ""; voltage: ""; batteryLevel: ""; current: false
    }

    function addRadio(radio) {
      // prevent multiple entries on device discovery problems
      for (var i = 0; i < count; i++) {
        if (get(i).name === radio.name) {
          return
        }
      }
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
      color: (index === view.currentIndex) ? "firebrick" : (model.ini ? "#808080" : "steelblue")
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
          font.family: fontText.name
          font.weight: Font.DemiBold
          color: "black"
          text: model.name
        }
      }

      Text {
        x: 58
        anchors.verticalCenter: parent.verticalCenter
        font.pixelSize: 18
        font.family: fontText.name
        font.weight: Font.DemiBold
        color: "white"
        text: model.hwModel
      }

      Rad.BatteryLevel {
        anchors.right: parent.right
        anchors.rightMargin: 14
        voltage: model.voltage
        level: model.batteryLevel
        visible: !model.ini
      }

      MouseArea {
        anchors.fill: parent

        onClicked: {
          if (index > 0) { // current radio is 0
            view.currentIndex = index
            Lisp.call("radios:change-radio", model.name)
          }
        }
      }
    }
  }
}
