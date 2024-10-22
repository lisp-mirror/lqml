import QtQuick
import "../" as Ext

Image {
  id: player

  Behavior on x {
    Ext.NumberAnimation {
      duration: 120
      easing.type: Easing.InOutSine
    }
  }

  Behavior on y {
    Ext.NumberAnimation {
      duration: 120
      easing.type: Easing.InOutSine
    }
  }

  // final animation

  Ext.RotationAnimation {
    objectName: "rotate_player"
    target: player
    property: "rotation"
    from: 0; to: 360
    duration: 600
  }
}
