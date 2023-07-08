import QtQuick 2.15
import QtQuick.Controls 2.15
import "." as Ext

Rectangle {
  id: rect
  color: "#b3cde3"

  Row {
    id: rowRegion
    padding: 9
    spacing: 9

    Ext.ComboBox {
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

      Ext.BatteryLevel {
        anchors.right: parent.right
        anchors.rightMargin: 14
        level: model.batteryLevel
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
