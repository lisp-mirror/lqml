import QtQuick

Rectangle {
  anchors.verticalCenter: parent.verticalCenter
  width: 12
  height: 25
  color: (percent() > 15) ? "#f0f0f0" : "yellow"
  radius: 2
  border.width: 1
  border.color: "#808080"

  property string voltage
  property string level

  function percent() { return parseInt(level, 10) }

  Rectangle {
    x: 1
    width: parent.width - 2
    height: (parent.height - 2) * percent() / 100
    anchors.bottom: parent.bottom
    anchors.bottomMargin: 1
    color: (percent() > 15) ? "#28c940" : "#ff5f57"
  }

  Text {
    x: -4 - paintedWidth
    height: parent.height
    verticalAlignment: Text.AlignVCenter
    horizontalAlignment: Text.AlignRight
    font.pixelSize: 11
    font.family: fontText.name
    font.weight: Font.DemiBold
    color: "white"
    text: voltage + "\n" + level
  }
}

