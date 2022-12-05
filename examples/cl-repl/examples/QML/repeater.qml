// example taken/adapted from 'qmlcreator' (github)
//
// N.B. for use in REPL app, only use all-in-one files like this one

import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Window 2.15

Item {
  anchors.fill: parent

  Button {
    anchors.centerIn: parent
    z: 1
    text: "Close"
    onClicked: parent.visible = false
  }

  property int rectSize: 14 * Screen.logicalPixelDensity

  Component {
    id: colorRectComponent

    Rectangle {
      id: colorRect
      width: 100
      height: 100
      color: Qt.rgba(Math.random(), Math.random(), Math.random(), 1)
      antialiasing: true

      Behavior on color {
        ColorAnimation {
          duration: 1000
        }
      }

      Timer {
        interval: 3000 * Math.random()
        repeat: true
        running: true
        triggeredOnStart: true
        onTriggered: parent.color = Qt.rgba(Math.random(), Math.random(), Math.random(), 1)
      }

      MouseArea {
        anchors.fill: parent
        onClicked: {
          if (!animation.running) {
            colorRect.z = 2
            animation.start()
          }
        }
      }

      RotationAnimation on rotation {
        id: animation
        from: 0
        to: (Math.random() > 0.5) ? 360 : -360
        duration: 1000
        running: false
        loops: 1
        onStopped: colorRect.z = 1
      }
    }
  }

  Grid {
    anchors.centerIn: parent
    columns: parent.width / rectSize
    rows: parent.height / rectSize

    Repeater {
      model: parent.columns * parent.rows
      delegate: Loader {
        sourceComponent: colorRectComponent
        width: rectSize
        height: rectSize
      }
    }
  }
}
