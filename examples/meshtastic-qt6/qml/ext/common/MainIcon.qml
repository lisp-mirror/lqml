import QtQuick

Image {
  horizontalAlignment: Image.AlignHCenter
  verticalAlignment: Image.AlignVCenter
  width: header.height
  height: width

  MouseArea {
    anchors.fill: parent
    onClicked: swipeView.currentIndex = parent.Positioner.index
    onPressAndHold: Lisp.call("app:icon-press-and-hold", parent.objectName)
  }
}
