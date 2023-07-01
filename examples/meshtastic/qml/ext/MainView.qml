import QtQuick 2.15
import QtQuick.Controls 2.15
import "." as Ext

Item {
  anchors.fill: parent

  Rectangle {
    id: header
    width: parent.width
    height: main.headerHeight
    color: "#f2f2f2"

    Row {
      height: parent.height
      spacing: 5
      anchors.horizontalCenter: parent.horizontalCenter

      Ext.MainIcon {
        source: "../img/group.png"

        Rectangle {
          objectName: "unread_messages"
          width: 10
          height: width
          anchors.right: parent.right
          anchors.top: parent.top
          anchors.margins: 7
          radius: width / 2
          color: "#ff5f57"
          visible: false
        }
      }

      Ext.MainIcon {
        source: "../img/message.png"
      }

      Ext.MainIcon {
        source: "../img/radio.png"
      }
    }
  }

  SwipeView {
    id: view
    objectName: "main_view"
    y: header.height
    width: parent.width
    height: parent.height - header.height
    currentIndex: 1
    interactive: false

    Ext.Group {}
    Ext.Messages {}
    Ext.Radios {}

    onCurrentIndexChanged: Lisp.call("app:view-index-changed", currentIndex)
  }

  PageIndicator {
    id: control
    y: header.height - 12
    count: view.count
    currentIndex: view.currentIndex
    anchors.horizontalCenter: parent.horizontalCenter

    delegate: Rectangle {
      width: header.height
      height: 5
      radius: width / 2
      color: "dodgerblue"
      opacity: (index === control.currentIndex) ? 1 : 0

      Behavior on opacity { OpacityAnimator { duration: 500 }}
    }
  }
}