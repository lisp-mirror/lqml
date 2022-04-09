import QtQuick 2.15
import QtQuick.Controls 2.15

Button {
  id: parenButton
  width: main.small ? 35 : 55
  height: width
  focusPolicy: Qt.NoFocus
  flat: true
  opacity: 0.12

  property url source

  background: Image {
    source: parent.source
  }

  onPressed: Lisp.call(this, "editor:button-pressed")
}
