import QtQuick
import QtQuick.Controls
import QtQuick.Controls.Basic

Button {
  id: parenButton
  width: 1.4 * (main.small ? 35 : 55)
  icon.width: width / 1.4
  icon.height: height / 1.4
  height: width
  focusPolicy: Qt.NoFocus
  flat: true
  opacity: 0.12

  onPressed: Lisp.call(this, "editor:button-pressed")
}
