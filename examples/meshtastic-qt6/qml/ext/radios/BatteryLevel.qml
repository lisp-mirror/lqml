import QtQuick

Rectangle {
  anchors.verticalCenter: parent.verticalCenter
  width: 12
  height: 22
  color: (level > 15) ? "#f0f0f0" : "yellow"
  radius: 2
  border.width: 1
  border.color: "#808080"

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
    font.pixelSize: 12
    font.family: fontText.name
    font.weight: Font.DemiBold
    color: "white"
    text: level + "%"
  }
}

