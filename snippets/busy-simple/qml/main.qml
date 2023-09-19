import QtQuick 2.15
import QtQuick.Controls 2.15

Item {
  width: 300
  height: 500

  AnimatedImage {
    objectName: "busy"
    anchors.centerIn: parent
    width: 64
    height: width
    z: 10
    source: "img/busy.gif"
    visible: playing
    playing: false
  }
}
