import QtQuick 2.15
import "." as Ext

Rectangle {
  color: "black"

  Ext.Gauge {}

  Column {
    anchors.horizontalCenter: parent.horizontalCenter
    anchors.bottom: parent.bottom
    anchors.bottomMargin: 10
    spacing: -5

    Ext.LogText { objectName: "distance"; font.pixelSize: 48 }
    Ext.LogText { objectName: "accuracy"; font.pixelSize: 24 }
  }
}
