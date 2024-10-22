import QtQuick
import QtQuick.Controls
import QtQuick.Controls.Basic
import QtLocation
import QtPositioning
import "." as Ext

Item {
  anchors.fill: parent

  Component {
    id: mapComponent

    Item {
      anchors.fill: parent

      property bool manualLocation: false
      property var myMarker: null

      Map {
        id: map
        objectName: "map"
        anchors.fill: parent
        plugin: mapPlugin
        zoomLevel: 14

        property geoCoordinate startCentroid

        SequentialAnimation {
          id: markerAnimation
          loops: Animation.Infinite
          running: manualLocation && (myMarker !== null)

          OpacityAnimator { target: myMarker; from: 1.0; to: 0.2; duration: 500; easing.type: Easing.InOutSine }
          OpacityAnimator { target: myMarker; from: 0.2; to: 1.0; duration: 500; easing.type: Easing.InOutSine }
        }

        MouseArea {
          anchors.fill: parent

          onClicked: (mouse) => {
            if (manualLocation) {
              manualLocation = false
              var coord = map.toCoordinate(Qt.point(mouse.x, mouse.y))
              myMarker.coordinate = coord
              myMarker.opacity = 1
              Lisp.call("loc:position-selected", coord.latitude, coord.longitude)
            }
          }
        }

        function coordinate(pos) {
          return QtPositioning.coordinate(pos[0], pos[1])
        }

        function setCenter(pos) {
          center = coordinate(pos)
        }

        function showMarker(n, nodeNum, name, customName = "") {
          var pos = Lisp.call("loc:position*", nodeNum)
          if (pos) {
            var marker = markers.itemAt(n)
            marker.radioName = name
            marker.customName = (customName === "~") ? "" : customName
            marker.coordinate = coordinate(pos)
            marker.visible = true
          }
        }

        function updatePositions(myNum, myName, group) {
          var n = 0
          showMarker(n++, myNum, myName)
          for (var i = 1; i < group.count; i++) {
            var data = group.get(i)
            showMarker(n++, data.nodeNum, data.radioName, data.customName)
          }
        }

        Plugin {
          id: mapPlugin
          name: "osm" // Open Street Map

          // for downloading tiles
          PluginParameter {
            name: "osm.mapping.cache.directory"
            value: Lisp.call("loc:tile-path")
          }
          // for offline tiles (from cache)
          PluginParameter {
            name: "osm.mapping.offline.directory"
            value: Lisp.call("loc:tile-path")
          }
          // number tiles (instead of MB)
          PluginParameter {
            name: "osm.mapping.cache.disk.cost_strategy"
            value: "unitary"
          }
          // max number cached/offline tiles
          PluginParameter {
            name: "osm.mapping.cache.disk.size"
            value: 10000
          }
          // local tile provider (no API key needed)
          PluginParameter {
            name: "osm.mapping.providersrepository.address"
            value: Lisp.call("loc:tile-provider-path")
          }
        }

        // handlers and shortcuts taken from Qt6 minimal map example
        PinchHandler {
          id: pinch
          target: null
          onActiveChanged: if (active) {
            map.startCentroid = map.toCoordinate(pinch.centroid.position, false)
          }
          onScaleChanged: (delta) => {
            map.zoomLevel += Math.log2(delta)
            map.alignCoordinateToPoint(map.startCentroid, pinch.centroid.position)
          }
          onRotationChanged: (delta) => {
            map.bearing -= delta
            map.alignCoordinateToPoint(map.startCentroid, pinch.centroid.position)
          }
          grabPermissions: PointerHandler.TakeOverForbidden
        }
        WheelHandler {
          id: wheel
          // workaround
          acceptedDevices: Qt.platform.pluginName === "cocoa" || Qt.platform.pluginName === "wayland"
                           ? PointerDevice.Mouse | PointerDevice.TouchPad
                           : PointerDevice.Mouse
          rotationScale: 1/120
          property: "zoomLevel"
        }
        DragHandler {
          id: drag
          target: null
          onTranslationChanged: (delta) => map.pan(-delta.x, -delta.y)
        }
        Shortcut {
          enabled: map.zoomLevel < map.maximumZoomLevel
          sequence: StandardKey.ZoomIn
          onActivated: map.zoomLevel = Math.round(map.zoomLevel + 1)
        }
        Shortcut {
          enabled: map.zoomLevel > map.minimumZoomLevel
          sequence: StandardKey.ZoomOut
          onActivated: map.zoomLevel = Math.round(map.zoomLevel - 1)
        }

        // node markers

        Ext.Markers {
          id: markers
          objectName: "markers"
        }
      }

      // manual marker buttons

      Ext.MapButton {
        id: hand
        objectName: "add_manual_marker"
        anchors.top: parent.top
        icon.source: "../../img/hand.png"
        visible: false

        onClicked: {
          manualLocation = !manualLocation
          if (manualLocation) {
            if (markers.count === 0) {
              Lisp.call("loc:add-manual-marker")
            }
            myMarker = markers.itemAt(0)
          } else {
            myMarker = null
          }
        }
      }

      Ext.MapButton {
        objectName: "remove_marker"
        anchors.top: hand.bottom
        icon.source: "../../img/remove-marker.png"

        onClicked: {
          markers.itemAt(0).visible = false
          Lisp.call("loc:remove-marker")
          visible = false
        }
      }
    }
  }

  Loader {
    id: mapLoader
    objectName: "map_loader"
    anchors.fill: parent
    sourceComponent: mapComponent
    active: false
  }
}
