import QtQuick 2.15
import QtQuick.Controls 2.15

Item {
  id: main
  width: 300
  height: 500

  Image {
    anchors.centerIn: parent
    anchors.horizontalCenterOffset: 5
    source: "https://common-lisp.net/static/imgs/lisplogo_flag2_128.png"
  }

  Label {
    objectName: "label"
    anchors.bottom: main.bottom
    anchors.bottomMargin: 20
    anchors.horizontalCenter: main.horizontalCenter
    text: "downloading..."
    font.pixelSize: 24
    font.bold: true
  }
}
