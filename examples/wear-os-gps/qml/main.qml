import QtQuick 2.15
import QtQuick.Controls 2.15
import QtSensors 5.15
import QtPositioning 5.15
import "ext/" as Ext

Rectangle {
  width: 210 // for desktop
  height: 210
  color: "black"

  PositionSource {
    objectName: "position_source"
    updateInterval: 1000
    active: true

    onPositionChanged: {
      Lisp.call("gps:position-changed",
                position.latitudeValid           ? position.coordinate.latitude  : null,
                position.longitudeValid          ? position.coordinate.longitude : null,
                position.horizontalAccuracyValid ? position.horizontalAccuracy   : null,
                position.speedValid              ? position.speed                : null,
                position.directionValid          ? position.direction            : null,
                position.timestamp.toLocaleString(Qt.locale(), "yyyy-MM-dd hh:mm:ss"))
    }
  }

  Ext.Gauge {}

  Column {
    anchors.horizontalCenter: parent.horizontalCenter
    anchors.bottom: parent.bottom
    anchors.bottomMargin: 5
    spacing: -5

    Ext.LogText { objectName: "distance"; font.pixelSize: 48 }
    Ext.LogText { objectName: "accuracy"; font.pixelSize: 24 }
  }

  // quit

  Keys.onPressed: {
    if (event.key === Qt.Key_Back) {
      event.accepted = true
      Lisp.call("gps:closing")
    }
  }
}
