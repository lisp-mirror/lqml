import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Window 2.15
import "ext/" as Ext

Item {
  id: main
  objectName: "main"
  width: 300
  height: 500

  property double headerHeight: 48

  Ext.MainView {}

  Image {
    source: "img/logo-128.png"
    width: headerHeight
    height: width
  }

  Image {
    source: "img/settings.png"
    width: headerHeight
    height: width
    anchors.right: parent.right
  }

  // shown while loading app (may take a while)
  Item {
    visible: loading.visible
    anchors.fill: parent

    AnimatedImage {
      id: loading
      objectName: "loading"
      anchors.centerIn: parent
      source: "img/busy.webp"
      visible: playing
      playing: true
    }

    Text {
      id: iniCount
      anchors.centerIn: parent
      color: "white"
      font.family: fontText.name
      font.pixelSize: 22
    }

    Timer {
      running: loading.playing
      interval: 4500
      repeat: true
      onTriggered: iniCount.text = Number(iniCount.text) + 1
    }
  }

  AnimatedImage {
    objectName: "busy"
    anchors.centerIn: parent
    width: 42
    height: width
    z: 10
    source: "img/busy.gif"
    visible: playing
    playing: false
  }

  FontLoader { id: fontText;  source: "fonts/tahoma.ttf" }
  FontLoader { id: fontMono;  source: "fonts/Hack-Regular.ttf" }
  FontLoader { id: fontMono2; source: "fonts/Hack-Bold.ttf" }
}
