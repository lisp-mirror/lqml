import QtQuick 2.15
import QtQuick.Controls 2.15
import "ext/" as Ext

Rectangle {
  width: 400
  height: 650
  id: main
  objectName: "main"
  color: "#bbb"

  function log(message) {
    logPage.log(message)
  }

  SwipeView {
    id: view
    objectName: "view"
    anchors.fill: parent

    // page 1: webview (native on mobile)
    Ext.Browser {}

    // page 2: log, repl
    Ext.Log { id: logPage }
  }

  PageIndicator {
    anchors.bottom: view.bottom
    anchors.bottomMargin: 10
    anchors.horizontalCenter: parent.horizontalCenter
    count: view.count
    currentIndex: view.currentIndex
  }

  Keys.onPressed: {
    if(event.key === Qt.Key_Back) {
      event.accepted = true
      if (view.currentIndex === 0) {
        Lisp.call("qml:qquit")
      } else {
        view.currentIndex--
      }
    }
  }

  FontLoader { id: fontIcons;    source: "fonts/fontawesome-webfont.ttf" }
  FontLoader { id: fontHack;     source: "fonts/Hack-Regular.ttf" }
  FontLoader { id: fontHackBold; source: "fonts/Hack-Bold.ttf" }
}
