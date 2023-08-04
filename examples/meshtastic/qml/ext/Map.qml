import QtQuick 2.15
import QtLocation 5.15
import QtPositioning 5.15
import "." as Ext

Item {
  anchors.fill: parent

  Component {
    id: mapComponent

    Map {
      id: map
      objectName: "map"
      anchors.fill: parent
      plugin: mapPlugin
      zoomLevel: 14

      function coordinate(pos) {
        return QtPositioning.coordinate(pos[0], pos[1])
      }

      function setCenter(pos) {
        center = coordinate(pos)
      }

      function updatePositions(group) {
        var n = 0
        for (var i = 0; i < group.count; i++) {
          var data = group.get(i)
          var pos = Lisp.call("loc:position*", data.nodeNum)
          if (pos) {
            var marker = markers.itemAt(n++)
            marker.radioName = data.radioName
            marker.customName = data.customName
            marker.coordinate = coordinate(pos)
            marker.visible = true
          }
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
        // local tile web-server (no API key needed), see 'cpp/tile_server.h'
        PluginParameter {
          name: "osm.mapping.providersrepository.address"
          value: "http://" + Lisp.call("app:my-ip") + ":1702/"
        }
      }

      Ext.Markers {
        id: markers
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
