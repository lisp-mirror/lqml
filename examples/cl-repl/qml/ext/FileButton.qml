import QtQuick 2.15
import QtQuick.Controls 2.15

Button {
  width: main.small ? 42 : 48
  height: width
  font.family: fontAwesome.name
  font.pixelSize: 24
  flat: true
}
