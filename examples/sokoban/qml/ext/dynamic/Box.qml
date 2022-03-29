import QtQuick 2.15
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
