import QtQuick
import QtQuick.Controls
import QtQuick.Controls.Basic

Button {
  width: main.small ? 30 : 42
  height: width
  font.family: fontAwesome.name
  font.pixelSize: main.small ? 20 : 28
  focusPolicy: Qt.NoFocus

  onPressed: Lisp.call(this, "editor:button-pressed")
}
