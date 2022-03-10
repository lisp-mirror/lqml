import QtQuick 2.15

Item {
  Rectangle {
    anchors.centerIn: parent
    width: Math.min(parent.width, parent.height)
    height: width
    radius: width
    color: Qt.lighter("blue", 1.7)
    border.width: 10
    border.color: "blue"

    Text {
      anchors.centerIn: parent
      text: "<h2>page 3</h2>"
    }
  }
}
