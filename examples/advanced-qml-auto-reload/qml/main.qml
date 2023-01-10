import QtQuick 2.15
import QtQuick.Controls 2.15
import ".ext/" as Ext        // for single file auto reload (development)
//import "ext/" as Ext       // release version

Rectangle {
  id: main
  width: 300
  height: 500
  objectName: "main"
  color: "black"

  SwipeView {
    id: view
    objectName: "view"
    anchors.fill: parent

    Rectangle {
      color: "white"

      Ext.Repl {}

      Text {
        anchors.centerIn: parent
        text: "swipe for next page"
      }
    }

    // N.B. don't use Loader inside a Repeater here, won't work with single
    // file auto reload (which already uses a Loader)

    Ext.Page1 {}
    Ext.Page2 {}
    Ext.Page3 {}
  }

  PageIndicator {
    anchors.bottom: view.bottom
    anchors.bottomMargin: 10
    anchors.horizontalCenter: parent.horizontalCenter
    count: view.count
    currentIndex: view.currentIndex
  }

  FontLoader { id: fontIcons;    source: "fonts/fontawesome-webfont.ttf" }
  FontLoader { id: fontHack;     source: "fonts/Hack-Regular.ttf" }
  FontLoader { id: fontHackBold; source: "fonts/Hack-Bold.ttf" }
}
