import QtQuick 2.15

Item {
  Rectangle {
    anchors.fill: parent
    radius: 100
    color: Qt.lighter("green", 3.0)
    border.width: 10
    border.color: "green"

    Text {
      anchors.centerIn: parent
      text: "<h2>page 2</h2>"
    }
  }
}
