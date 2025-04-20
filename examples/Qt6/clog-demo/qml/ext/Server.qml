import QtQuick
import QtWebSockets
import "." as Ext

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

    onClientConnected: (webSocket) => {
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
      })
    }

    onErrorStringChanged: {
      main.log("[server error] " + errorString)
    }
  }
}
