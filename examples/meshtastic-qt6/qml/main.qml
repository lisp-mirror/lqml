import QtQuick
import QtQuick.Controls
import QtQuick.Controls.Basic
import QtQuick.Window
import QtPositioning
import "ext/common/" as Com
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

  Com.MainView { id: view }

  Com.Menu {
    id: menu
    objectName: "menu"

    function show() { popup(0, headerHeight) }

    Com.MenuItem {
      objectName: "help"
      text: qsTr("Help")
      onTriggered: help.active ? help.item.enabled = !help.item.enabled : help.active = true
    }

    Com.MenuItem {
      text: qsTr("Update group/nodes")
      onTriggered: Lisp.call("lora:get-node-config")
      enabled: (view.pageIndex === 0)
    }

    Com.MenuItem {
      text: qsTr("Channel name...")
      onTriggered: Lisp.call("lora:edit-channel-name")
      enabled: (view.pageIndex === 0)
    }

    Com.MenuItem {
      text: qsTr("Message font size...")
      onTriggered: Lisp.call("msg:font-size-dialog")
      enabled: (view.pageIndex === 1)
    }

    MenuSeparator {}

    Com.MenuItem {
      objectName: "share_location"
      text: qsTr("Share my location")
      checkable: true
      onTriggered: Lisp.call("loc:share-my-location", checked)
    }

    MenuSeparator {}

    Com.Menu {
      id: connection
      title: qsTr("Connection")
      enabled: (view.pageIndex === 2)

      function changed(name) { Lisp.call("radios:connection-changed", name) }

      Com.MenuItem {
        objectName: "BLE"
        text: "BLE"
        autoExclusive: true
        checkable: true
        checked: true
        onTriggered: connection.changed(objectName)
      }
      Com.MenuItem {
        objectName: "USB"
        text: "USB"
        autoExclusive: true
        checkable: true
        onTriggered: connection.changed(objectName)
      }
      Com.MenuItem {
        objectName: "WIFI"
        text: "WiFi"
        autoExclusive: true
        checkable: true
        onTriggered: connection.changed(objectName)
        Component.onCompleted: if (Qt.platform.os === "ios") { palette.windowText = "crimson" }
      }
    }

    Com.MenuItem {
      text: qsTr("Reset node DB")
      onTriggered: Lisp.call("lora:reset-node-db")
      enabled: (view.pageIndex === 0)
    }

    Com.MenuItem {
      text: qsTr("Export message DB (Lisp)")
      onTriggered: Lisp.call("db:export-to-list")
      enabled: (view.pageIndex === 1)
    }

    Com.MenuItem {
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
    visible: (view.pageIndex === 0)

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
    visible: (view.pageIndex === 1)

    MouseArea {
      anchors.fill: parent
      onClicked: Lisp.call("msg:find-clicked")
    }
  }

  Com.Hourglass { // animation while loading app
    id: hourglass
  }

  AnimatedImage {
    objectName: "busy"
    anchors.centerIn: parent
    width: 34
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
    property double alt: 0
    property string time: "0" // no 'long' in JS

    onPositionChanged: {
      if (position.latitudeValid && position.longitudeValid) {
        var coor = position.coordinate;
        lat = coor.latitude
        lon = coor.longitude
        alt = position.altitudeValid ? coor.altitude : 0
        if (position.timestamp) {
          var stime = String(position.timestamp.getTime())
          time = stime.substring(0, stime.length - 3)
        } else {
          time = "0"
        }
      }
    }

    function lastPosition() {
      return [lat, lon, alt, time]
    }
  }

  Com.Toast {}

  Dlg.Dialogs {}

  Loader {
    id: help
    y: headerHeight
    width: parent.width
    height: parent.height - headerHeight
    source: "ext/common/Help.qml"
    active: false
  }

  FontLoader { id: fontText;  source: "fonts/Ubuntu.ttf" }
  FontLoader { id: fontText2; source: "fonts/Ubuntu-Medium.ttf" }
}
