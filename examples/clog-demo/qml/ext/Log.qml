import QtQuick 2.15
import QtQuick.Controls 2.15
import QtWebSockets 1.15
import "." as Ext

Rectangle {
  id: page
  color: "lavender"

  Ext.Repl {}

  // iOS only
  WebSocketServer {
    id: server
    objectName: "server"
    port: 8080
    listen: (Qt.platform.os === "ios")

    property int counter: 0
    property var client

    function send(message) {
      client.sendTextMessage(message)
    }

    onClientConnected: {
      client = webSocket
      webSocket.objectName = ++counter
      page.log("[new] " + counter)
      Lisp.call("clog:webview/on-new-connection")

      webSocket.onTextMessageReceived.connect(function(message) {
        page.log(message)
        Lisp.call("clog:webview/on-message", message)
      })

      webSocket.onStatusChanged.connect(function(status) {
        var state
        switch (status) {
          case WebSocket.Closed: state = "close"; break
          case WebSocket.Error:  state = "error"; break
          default: return
        }
        page.log("[status] " + state)
        if (status === WebSocket.Closed) {
          Lisp.call("clog:webview/on-close")
        }
      })
    }

    onErrorStringChanged: {
      page.log("[server error] " + errorString)
    }
  }

  function log(message) {
    logModel.append({ message: message })
    listView.positionViewAtEnd()
  }

  ListView {
    id: listView
    anchors.fill: parent
    model: ListModel { id: logModel }
    delegate: Text {
      font.pixelSize: 14
      text: message
    }
  }
}
