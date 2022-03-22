import QtQuick 2.15
import QtQuick.Controls 2.15
import "ext/" as Ext

Rectangle {
  width: 400
  height: 650
  objectName: "main"
  color: "#333"

  SwipeView {
    id: view
    objectName: "view"
    anchors.fill: parent

    // page 1: webview (native on mobile)
    Ext.Browser {}

    // page 2: websocket server, log, repl
    Ext.Server {}
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
