import QtQuick 2.15
import QtQuick.Controls 2.15

RoundButton {
  z: 1
  anchors.right: parent.right
  anchors.margins: 5
  icon.color: "#eee"
  icon.width: 28
  width: 38
  height: width
  radius: width / 2
  palette.button: "#555"
}
