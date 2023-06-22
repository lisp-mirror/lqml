import QtQuick 2.15

Rectangle {
  anchors.verticalCenter: parent.verticalCenter
  width: 10
  height: 25
  color: "#f0f0f0"
  radius: 2
  border.width: 1
  border.color: "#505050"

  property int level: 0

  Rectangle {
    x: 1
    width: parent.width - 2
    height: (parent.height - 2) * level / 100
    anchors.bottom: parent.bottom
    anchors.bottomMargin: 1
    color: (level > 15) ? "#28c940" : "#ff5f57"
  }

  Text {
    x: -4 - paintedWidth
    height: parent.height
    verticalAlignment: Text.AlignVCenter
    font.pixelSize: 10
    font.bold: true
    font.family: fontMono.name
    color: "white"
    text: level + "%"
  }
}

