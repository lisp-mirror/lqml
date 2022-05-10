import QtQuick 2.15
import "ext/" as Ext

Rectangle {
  width: 210  // for desktop
  height: 210
  color: "black"

  Ext.HeartRate {}

  Keys.onPressed: {
    if (event.key === Qt.Key_Back) {
      event.accepted = true
      Lisp.call("qml:qquit")
    }
  }
}
