import QtQuick 2.15

Image {
  horizontalAlignment: Image.AlignHCenter
  verticalAlignment: Image.AlignVCenter
  width: header.height
  height: width

  MouseArea {
    anchors.fill: parent
    onClicked: view.currentIndex = parent.Positioner.index
  }
}
