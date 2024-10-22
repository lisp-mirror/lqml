import QtQuick
import "../" as Ext

Image {
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
}
