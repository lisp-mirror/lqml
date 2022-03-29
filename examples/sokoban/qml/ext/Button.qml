import QtQuick 2.15
import QtQuick.Controls 2.15

Button {
  width: main.small ? 32 : 50
  height: width
  font.family: fontAwesome.name
  font.pixelSize: width - 6
  opacity: 0.8

  onPressed: Lisp.call(this, "qsoko:button-pressed")
}
