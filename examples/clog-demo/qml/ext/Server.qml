import QtQuick 2.15
import QtWebSockets 1.15
import "." as Ext

// for iOS only

Item {
  WebSocketServer {
    id: server
    objectName: "server"
    port: 8080
    listen: true

    property int counter: 0
    property var client

    function send(message) {
      client.sendTextMessage(message)
    }

    onClientConnected: {
      client = webSocket
      webSocket.objectName = ++counter
      main.log("[new] " + counter)
      Lisp.call("clog:webview/on-new-connection")

      webSocket.onTextMessageReceived.connect(function(message) {
        main.log(message)
        Lisp.call("clog:webview/on-message", message)
      })

      webSocket.onStatusChanged.connect(function(status) {
        var state
        switch (status) {
          case WebSocket.Closed: state = "close"; break
          case WebSocket.Error:  state = "error"; break
          default: return
        }
        main.log("[status] " + state)
        if (status === WebSocket.Closed) {
          Lisp.call("clog:webview/on-close")
        }
      })
    }

    onErrorStringChanged: {
      main.log("[server error] " + errorString)
    }
  }
}
