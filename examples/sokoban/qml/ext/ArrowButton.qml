import QtQuick 2.15
import QtQuick.Controls 2.15

Button {
  width: main.small ? 37 : 50
  height: width
  flat: true
  focusPolicy: Qt.NoFocus
  font.family: fontAwesome.name
  font.pixelSize: 1.2 * width
  opacity: 0.2
  scale: 1.2

  onPressed: Lisp.call(this, "qsoko:button-pressed")
}
