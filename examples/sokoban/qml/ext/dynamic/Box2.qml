import QtQuick 2.15
import "../" as Ext

Image {
  id: box2

  Behavior on x {
    Ext.NumberAnimation {
      duration: 150
      easing.type: Easing.InQuart
    }
  }

  Behavior on y {
    Ext.NumberAnimation {
      duration: 150
      easing.type: Easing.InQuart
    }
  }

  // final animation

  Ext.SequentialAnimation {
    objectName: "wiggle_box"
    loops: 3

    RotationAnimation {
      target: box2
      property: "rotation"
      from: 0; to: 30
      duration: 150
    }

    RotationAnimation {
      target: box2
      property: "rotation"
      from: 30; to: -30
      duration: 300
    }

    RotationAnimation {
      target: box2
      property: "rotation"
      from: -30; to: 0
      duration: 150
    }
  }
}
