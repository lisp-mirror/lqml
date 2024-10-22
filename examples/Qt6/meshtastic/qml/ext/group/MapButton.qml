import QtQuick
import QtQuick.Controls
import QtQuick.Controls.Basic

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
  focusPolicy: Qt.NoFocus
}
