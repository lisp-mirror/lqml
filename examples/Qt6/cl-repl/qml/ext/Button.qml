import QtQuick
import QtQuick.Controls
import QtQuick.Controls.Basic

Button {
  width: main.small ? 40 : 60
  height: main.small ? 37 : 55
  font.family: fontAwesome.name
  font.pixelSize: main.small ? 25 : 36
  focusPolicy: Qt.NoFocus

  onPressed: Lisp.call(this, "editor:button-pressed")
}
