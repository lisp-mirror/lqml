import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Window 2.15
import "ext/" as Ext

Item {
  id: main
  objectName: "main"
  width: 350
  height: 550

  property double headerHeight: 48

  Ext.MainView { id: view }

  Image {
    source: "img/logo-128.png"
    width: headerHeight
    height: width
  }

  Image {
    objectName: "find"
    source: "img/find.png"
    width: headerHeight
    height: width
    anchors.right: parent.right

    MouseArea {
      anchors.fill: parent
      onClicked: Lisp.call("msg:find-clicked")
    }
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

  Ext.Toast {}

  FontLoader { id: fontText;  source: "fonts/Ubuntu.ttf" }
  FontLoader { id: fontText2; source: "fonts/Ubuntu-Medium.ttf" }
}
