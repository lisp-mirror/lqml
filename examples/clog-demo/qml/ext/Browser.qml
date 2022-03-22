import QtQuick 2.15
import QtQuick.Controls 2.15
import QtWebView 1.15

Item {
  WebView {
    id: browser
    objectName: "browser"
    width: parent. width
    height: parent.height - reload.height
    visible: !busy.visible
  }

  Button {
    id: reload
    anchors.bottom: parent.bottom
    text: "Reload"
    onClicked: browser.reload()
  }

  Button {
    anchors.bottom: parent.bottom
    anchors.right: parent.right
    text: "log/REPL"
    onClicked: view.currentIndex = 1
  }

  Rectangle {
    id: busy
    objectName: "busy"
    color: "white"
    anchors.fill: parent

    Image {
      anchors.centerIn: parent
      source: "../img/busy.png"
    }
  }
}
