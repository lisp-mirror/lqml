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

    onLoadingChanged: {
      if (loadRequest.status === WebView.LoadSucceededStatus) {
        Lisp.call("clog:webview/on-new-connection")
      }
    }

    // hack to get notified from the browser, see 'boot.js'
    onTitleChanged: {
      if ((title !== "-") && (title !== "boot.html")) {
        Lisp.call("clog:webview/on-message", title)
        main.log(title)
      }
    }
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
