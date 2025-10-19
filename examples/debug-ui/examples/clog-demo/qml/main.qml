import QtQuick 2.15
import QtQuick.Controls 2.15
import "ext/" as Ext
import "debug/" as Dbg

StackView {
  id: main
  width: 400
  height: 650
  objectName: "main"
  initialItem: mainRect

  // show/hide dialogs

  function pushDialog(name)  {
    switch (name) {
      case "debug": main.push(dialogDebug); break
    }
  }

  function popDialog() { main.pop() }

  // log

  function log(message) {
    logPage.log(message)
  }

  FontLoader { id: fontIcons;    source: "fonts/fontawesome-webfont.ttf" }
  FontLoader { id: fontHack;     source: "fonts/Hack-Regular.ttf" }
  FontLoader { id: fontHackBold; source: "fonts/Hack-Bold.ttf" }

  Rectangle {
    id: mainRect
    color: "#bbb"

    SwipeView {
      id: view
      objectName: "view"
      anchors.fill: parent

      // page 1: webview (native on mobile)
      Ext.Browser {}

      // page 2: log
      Ext.Log { id: logPage }
    }

    PageIndicator {
      anchors.bottom: view.bottom
      anchors.bottomMargin: 10
      anchors.horizontalCenter: parent.horizontalCenter
      count: view.count
      currentIndex: view.currentIndex
    }

    // dialogs

    Dbg.DebugDialog { id: dialogDebug }
  }

  Keys.onPressed: (event) => {
    if (event.key === Qt.Key_Back) {
      event.accepted = true
      if (view.currentIndex === 0) {
        Lisp.call("qml:qquit")
      } else {
        view.currentIndex--
      }
    }
  }
}
