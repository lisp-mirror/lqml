import QtQuick 2.15
import QtQuick.Controls 2.15
import QtSensors 5.15

Rectangle {
  width: 400
  height: 600
  color: "#333"

  Rectangle {
    anchors.centerIn: parent
    anchors.verticalCenterOffset: -25
    width: grid.width
    height: grid.height
    color: "white"

    Rectangle {
      objectName: "ball"
      width: 13
      height: width
      radius: width / 2
      color: "#c33"
    }
  }

  Grid {
    id: grid
    anchors.centerIn: parent
    anchors.verticalCenterOffset: -25
    columns: 25
    rows: 39
    columnSpacing: 1
    rowSpacing: 1

    Repeater {
      id: maze
      objectName: "maze"
      model: parent.columns * parent.rows

      Rectangle {
        width: 12
        height: width
        color: "steelblue"
        radius: width / 5
      }
    }
  }

  RoundButton {
    id: button
    anchors.bottom: parent.bottom
    anchors.bottomMargin: 7
    anchors.horizontalCenter: parent.horizontalCenter
    text: "Change Maze"

    onClicked: Lisp.call("maze:new-game")
  }

  // sensor and timer

  TiltSensor {
    id: tilt
    active: true
  }

  Timer {
    objectName: "timer"
    interval: 30
    repeat: true

    onTriggered: Lisp.call("maze:move", tilt.reading.xRotation, tilt.reading.yRotation)
  }
}
