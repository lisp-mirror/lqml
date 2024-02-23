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
  property bool mobile: Lisp.call("mobile-p")
  property bool broadcast: false

  function showKeyboard(show) {
    show ? Qt.inputMethod.show() : Qt.inputMethod.hide()
  }

  Ext.MainView { id: view }

  Ext.Menu {
    id: menu
    objectName: "menu"

    function show() { popup(0, headerHeight) }

    Ext.MenuItem {
      objectName: "help"
      text: qsTr("Help")
      onTriggered: help.active ? help.item.enabled = !help.item.enabled : help.active = true
    }

    Ext.MenuItem {
      text: qsTr("Channel name...")
      onTriggered: Lisp.call("lora:edit-channel-name")
      enabled: (view.currentIndex === 0)
    }

    Ext.MenuItem {
      text: qsTr("Update group/nodes")
      onTriggered: Lisp.call("lora:get-node-config")
      enabled: (view.currentIndex === 0)
    }

    Ext.MenuItem {
      text: qsTr("Message font size...")
      onTriggered: Lisp.call("msg:font-size-dialog")
      enabled: (view.currentIndex === 1)
    }

    MenuSeparator {}

    Ext.MenuItem {
      objectName: "share_location"
      text: qsTr("Share my location...")
      onTriggered: Lisp.call("loc:share-my-location")
    }

    MenuSeparator {}

    Ext.MenuItem {
      text: qsTr("Device filter...")
      onTriggered: Lisp.call("lora:edit-device-filter")
      enabled: (view.currentIndex === 2)
    }

    Ext.MenuItem {
      text: qsTr("Export DB (Lisp)")
      onTriggered: Lisp.call("db:export-to-list")
    }

    Ext.MenuItem {
      text: qsTr("Make backup")
      onTriggered: Lisp.call("app:make-backup")
      enabled: !mobile
    }
  }

  Image {
    source: "img/logo.png"
    x: 2
    y: 2
    width: headerHeight
    height: width

    MouseArea {
      anchors.fill: parent
      onClicked: menu.show()
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

  Ext.Hourglass { // animation while loading app
    id: hourglass
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

    property double lat: 0
    property double lon: 0
    property string time: "0" // no 'long' in JS

    onPositionChanged: {
      if (position.latitudeValid && position.longitudeValid) {
        var coor = position.coordinate;
        lat = coor.latitude
        lon = coor.longitude
        if (position.timestamp) {
          var stime = String(position.timestamp.getTime())
          time = stime.substring(0, stime.length - 3)
        } else {
          time = "0"
        }
      }
    }

    function lastPosition() {
      return [lat, lon, time]
    }
  }

  Ext.Toast {}

  Dlg.Dialogs {}

  Loader {
    id: help
    y: headerHeight
    width: parent.width
    height: parent.height - headerHeight
    source: "ext/Help.qml"
    active: false
  }

  FontLoader { id: fontText;  source: "fonts/Ubuntu.ttf" }
  FontLoader { id: fontText2; source: "fonts/Ubuntu-Medium.ttf" }
}
