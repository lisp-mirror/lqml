import QtQuick 2.15
import QtQuick.Controls 2.15

Item {
  width: 300
  height: 500

  Label {
    objectName: "label"
    text: "downloading..."
    anchors.centerIn: parent
    font.pixelSize: 24
    font.bold: true
  }
}
