import QtQuick 2.15

Rectangle {
  radius: 50
  color: Qt.lighter("blue", 1.7)
  border.width: 10
  border.color: "blue"

  Text {
    anchors.centerIn: parent
    text: "<h2>page 3</h2>"
  }
}
