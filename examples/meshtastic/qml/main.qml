import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Window 2.15
import QtPositioning 5.15
import "ext/" as Ext
import "ext/dialogs/" as Dlg

Item {
  id: rootItem
  objectName: "main"
  width: 350
  height: 550

  property double headerHeight: 48
  property bool mobile: (Qt.platform.os === "android") || (Qt.platform.os === "ios")

  function showKeyboard(show) {
    show ? Qt.inputMethod.show() : Qt.inputMethod.hide()
  }

  Ext.MainView { id: view }

  Menu {
    id: menu

    Ext.MenuItem {
      text: qsTr("Message font size...")
      onTriggered: Lisp.call("msg:font-size-dialog")
    }

    Ext.MenuItem {
      text: qsTr("Update group/nodes")
      onTriggered: Lisp.call("lora:get-node-config")
    }

    Ext.MenuItem {
      text: qsTr("Make backup")
      onTriggered: Lisp.call("app:make-backup")
      enabled: !mobile
    }
  }

  Image {
    source: "img/logo-128.png"
    x: 2
    y: 2
    width: headerHeight
    height: width

    MouseArea {
      anchors.fill: parent
      onClicked: menu.popup(0, headerHeight)
    }
  }

  Image { // location icon ('Group')
    objectName: "location"
    source: "img/location.png"
    width: headerHeight
    height: width
    anchors.right: parent.right
    visible: false

    MouseArea {
      anchors.fill: parent
      onClicked: Lisp.call("loc:show-map-clicked")
    }
  }

  Image { // find icon ('Messages')
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

  // GPS

  PositionSource {
    objectName: "position_source"
    updateInterval: 2000
    active: false

    property double lat:  0
    property double lon:  0
    property string time: "" // no 'long' in JS

    onPositionChanged: {
      if (position.latitudeValid && position.longitudeValid) {
        var coor = position.coordinate;
        lat = coor.latitude
        lon = coor.longitude
        time = coor.timestamp ? String(coor.timestamp) : ""
      }
    }

    function lastPosition() {
      return [lat, lon, time]
    }
  }

  Ext.Toast {}

  Dlg.Dialogs {}

  FontLoader { id: fontText;  source: "fonts/Ubuntu.ttf" }
  FontLoader { id: fontText2; source: "fonts/Ubuntu-Medium.ttf" }
}
