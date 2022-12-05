// example taken/adapted from 'qmlcreator' (github)
//
// N.B. for use in REPL app, only use all-in-one files like this one

import QtQuick 2.15
import QtQuick.Controls 2.15

Item {
  anchors.fill: parent

  Column {
    anchors.fill: parent

    Item {
      anchors.horizontalCenter: parent.horizontalCenter
      width: 110
      height: width

      Rectangle {
        anchors.centerIn: parent
        width: Math.min(parent.width, parent.height) - 20
        height: width
        radius: width / 2
        color: "#eaeaea"
        border.width: width * 0.03
        border.color: "black"

        Repeater {
          model: 12
          delegate: Item {
            width: parent.border.width
            height: parent.radius - width * 2
            rotation: 30 * (index + 1)
            transformOrigin: Item.Bottom
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.bottom: parent.verticalCenter

            Text {
              anchors.horizontalCenter: parent.horizontalCenter
              anchors.top: parent.top
              text: index + 1
              rotation: -parent.rotation
              font.pixelSize: parent.width * 4
            }
          }
        }

        Rectangle {
          id: hourHand
          width: parent.border.width * 0.5
          height: parent.radius - parent.border.width * 7
          antialiasing: true
          color: "black"
          transformOrigin: Item.Bottom
          anchors.horizontalCenter: parent.horizontalCenter
          anchors.bottom: parent.verticalCenter
        }

        Rectangle {
          id: minuteHand
          width: parent.border.width * 0.5
          height: parent.radius - parent.border.width * 2
          antialiasing: true
          color: "black"
          transformOrigin: Item.Bottom
          anchors.horizontalCenter: parent.horizontalCenter
          anchors.bottom: parent.verticalCenter
        }

        Rectangle {
          id: secondHand
          width: parent.border.width * 0.2
          height: parent.radius - parent.border.width * 2
          antialiasing: true
          color: "red"
          transformOrigin: Item.Bottom
          anchors.horizontalCenter: parent.horizontalCenter
          anchors.bottom: parent.verticalCenter
        }

        Rectangle {
          width: parent.width * 0.04
          height: width
          anchors.centerIn: parent
          radius: width / 2
          color: "black"
        }
      }

      Timer {
        running: parent.enabled
        interval: 200
        triggeredOnStart: true
        repeat: true
        onTriggered: {
          var currentTime = new Date()
          secondHand.rotation = currentTime.getSeconds() * 6
          minuteHand.rotation = currentTime.getMinutes() * 6 + currentTime.getSeconds() * 0.1
          hourHand.rotation = currentTime.getHours() * 30 + currentTime.getMinutes() * 0.5
        }
      }
    }

    Button {
      anchors.horizontalCenter: parent.horizontalCenter
      text: "Close"
      onClicked: parent.visible = false
    }
  }
}
