import QtQuick 2.15
import QtQuick.Controls 2.15
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

    property bool ready: false

    onPositionChanged: {
      if (ready) {
        Lisp.call("gps:position-changed",
                  position.latitudeValid           ? position.coordinate.latitude  : null,
                  position.longitudeValid          ? position.coordinate.longitude : null,
                  position.horizontalAccuracyValid ? position.horizontalAccuracy   : null,
                  position.speedValid              ? position.speed                : null,
                  position.directionValid          ? position.direction            : null,
                  position.timestamp.toLocaleString(Qt.locale(), "yyyy-MM-dd hh:mm:ss"))
      }
    }
  }

  SwipeView {
    id: view
    anchors.fill: parent
    orientation: Qt.Vertical

    onCurrentIndexChanged: {
      Lisp.call("gps:set-max-speed", currentIndex)
      if (currentIndex === 0) {
        Lisp.call("gps:save-settings")
      }
    }

    Ext.MainView {}
    Ext.Settings {}
  }

  PageIndicator {
    id: indicator
    anchors.bottom: view.bottom
    anchors.bottomMargin: 3
    anchors.horizontalCenter: parent.horizontalCenter
    height: 14
    count: view.count
    currentIndex: view.currentIndex

    delegate: Rectangle {
      implicitWidth: 6
      implicitHeight: 6
      radius: width / 2
      color: (view.currentIndex === 0) ? "white" : "black"
      opacity: index === indicator.currentIndex ? 1 : 0.35
    }
  }

  // quit

  Keys.onPressed: {
    if (event.key === Qt.Key_Back) {
      event.accepted = true
      Lisp.call("gps:closing")
    }
  }
}
