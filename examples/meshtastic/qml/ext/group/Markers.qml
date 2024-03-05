import QtQuick 2.15
import QtLocation 5.15

Repeater {
  model: Lisp.call("loc:position-count")

  MapQuickItem {
    anchorPoint.x: image.width / 2
    anchorPoint.y: image.height
    visible: false

    property alias radioName:  radioName.text
    property alias customName: customName.text

    sourceItem: Image {
      id: image
      width: 25
      height: width
      source: "../../img/marker.png"

      Rectangle {
        x: -(width - image.width) / 2
        y: image.height + 5
        width: customName.width + 42
        height: 20
        color: (index === 0) ? "#ff3d00" : "darkcyan"
        radius: height / 2

        Rectangle {
          x: 2
          anchors.verticalCenter: parent.verticalCenter
          width: 38
          height: 16
          color: "#f0f0f0"
          radius: height / 2

          Text {
            id: radioName
            anchors.centerIn: parent
            font.pixelSize: 12
            font.family: fontText.name
            font.weight: Font.DemiBold
            color: "black"
          }
        }

        Text {
          id: customName
          x: 44
          anchors.verticalCenter: parent.verticalCenter
          width: paintedWidth ? (paintedWidth + 10) : 0
          font.pixelSize: 14
          font.family: fontText.name
          font.weight: Font.DemiBold
          color: "white"
        }
      }
    }
  }
}

