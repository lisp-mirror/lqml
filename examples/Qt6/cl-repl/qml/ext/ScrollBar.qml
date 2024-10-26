// This is a modified version taken from the QML sources

import QtQuick
import QtQuick.Controls
import QtQuick.Controls.Basic

ScrollBar {
  id: control
  orientation: Qt.Vertical

  contentItem: Rectangle {
    implicitWidth: 12
    implicitHeight: 100
    radius: width / 2
    color: control.pressed ? "#202020" : "#909090"
    opacity: 0.0

    states: State {
      name: "active"
      when: (control.active && control.size < 1.0)
      PropertyChanges { target: control.contentItem; opacity: 0.75 }
    }

    transitions: Transition {
      from: "active"
      SequentialAnimation {
        PauseAnimation { duration: 450 }
        NumberAnimation { target: control.contentItem; duration: 200; property: "opacity"; to: 0.0 }
      }
    }
  }
}

