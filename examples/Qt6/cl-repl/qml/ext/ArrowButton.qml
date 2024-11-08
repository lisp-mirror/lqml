import QtQuick
import QtQuick.Controls
import QtQuick.Controls.Basic

Button {
  width: main.small ? 33 : 45
  height: width
  flat: true
  focusPolicy: Qt.NoFocus
  font.family: fontAwesome.name
  font.pixelSize: 1.2 * width
  opacity: 0.12
  scale: 1.2

  onPressed:      Lisp.call(this, "editor:button-pressed")
  onPressAndHold: Lisp.call(this, "editor:button-pressed-and-helt")
}
