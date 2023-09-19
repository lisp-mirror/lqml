import QtQuick 2.15

Rectangle {
  anchors.fill: parent
  color: "#ccebc5"
  visible: animation.running

  Image {
    id: hourglass1
    anchors.centerIn: parent
    width: 40
    fillMode: Image.PreserveAspectFit
    source: "../img/hourglass.png"
  }

  Image {
    id: hourglass2
    anchors.centerIn: parent
    width: hourglass1.width
    fillMode: Image.PreserveAspectFit
    source: "../img/hourglass.png"
    opacity: 0
  }

  SequentialAnimation {
    id: animation
    objectName: "hourglass"
    loops: Animation.Infinite
    running: true

    RotationAnimation { target: hourglass1; from: 0; to: 180; duration: 1000; easing.type: Easing.InOutSine }

    ParallelAnimation {
      NumberAnimation { target: hourglass1; property: "opacity"; from: 1; to: 0; duration: 1500; easing.type: Easing.InOutSine }
      NumberAnimation { target: hourglass2; property: "opacity"; from: 0; to: 1; duration: 1500; easing.type: Easing.InOutSine }
    }

    // reset
    NumberAnimation { target: hourglass1; property: "opacity"; to: 1; duration: 0 }
    NumberAnimation { target: hourglass2; property: "opacity"; to: 0; duration: 0 }
  }
}
