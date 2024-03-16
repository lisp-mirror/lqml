import QtQuick 2.15
import QtQuick.Controls 2.15
import "." as Ext
import "../group/" as Grp
import "../messages/" as Msg
import "../radios/" as Rad

Item {
  anchors.fill: parent

  property alias pageIndex: swipeView.currentIndex

  Rectangle {
    id: header
    width: parent.width
    height: rootItem.headerHeight
    color: "#f2f2f2"

    Row {
      height: parent.height
      spacing: 5
      anchors.horizontalCenter: parent.horizontalCenter

      Ext.MainIcon {
        objectName: "group_icon"
        source: "../../img/group.png"

        Rectangle {
          objectName: "unread_messages"
          width: 10
          height: width
          anchors.right: parent.right
          anchors.top: parent.top
          anchors.margins: 7
          radius: width / 2
          color: "#ff4040"
          visible: false
        }
      }

      Ext.MainIcon {
        objectName: "message_icon"
        source: "../../img/message.png"
      }

      Ext.MainIcon {
        objectName: "radio_icon"
        source: "../../img/radio.png"
      }
    }
  }

  SwipeView {
    id: swipeView
    objectName: "main_view"
    y: header.height
    width: parent.width
    height: parent.height - header.height
    currentIndex: 1
    interactive: false

    Grp.Group { id: group }
    Msg.Messages {}
    Rad.Radios {}

    onCurrentIndexChanged: Lisp.call("app:view-index-changed", currentIndex)
  }

  PageIndicator {
    id: control
    y: header.height - 12
    count: swipeView.count
    currentIndex: swipeView.currentIndex
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
