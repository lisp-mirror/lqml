import QtQuick 2.15

Item {
  Rectangle {
    anchors.fill: parent
    color: Qt.lighter("red", 1.5)
    border.width: 10
    border.color: "red"

    Text {
      anchors.centerIn: parent
      text: "<h2>page 1</h2>"
    }
  }
}
