import QtQuick
import QtQuick.Controls
import QtQuick.Controls.Basic

Button {
  width: main.small ? 42 : 48
  height: width
  font.family: fontAwesome.name
  font.pixelSize: 24
  flat: true
}
