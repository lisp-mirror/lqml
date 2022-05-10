import QtQuick 2.15

Rectangle {
  objectName: "main"
  anchors.fill: parent
  anchors.centerIn: parent
  radius: width / 2
  color: "black"
  border.width: 5
  border.color: "#e04040"

  Column {
    anchors.centerIn: parent

    Text {
      anchors.horizontalCenter: parent.horizontalCenter
      objectName: "heart_rate"
      color: "white"
      font.weight: Font.DemiBold
      font.pixelSize: 32
      text: "60"
    }

    Text {
      anchors.horizontalCenter: parent.horizontalCenter
      objectName: "accuracy"
      color: "white"
      font.pixelSize: 14
      text: "demo"
    }

    Image {
      id: heart
      width: 100
      height: width
      source: "../img/heart.png"

      SequentialAnimation {
        objectName: "animation"
        running: true

        ScaleAnimator {
          objectName: "zoom_in"
          target: heart
          from: 0.7; to: 1.0
          duration: 500
          easing.type: Easing.InCubic
        }

        ScaleAnimator {
          objectName: "zoom_out"
          target: heart
          from: 1.0; to: 0.7
          duration: 500
          easing.type: Easing.OutCubic
        }
      }
    }
  }
}
