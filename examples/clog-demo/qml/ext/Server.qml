import QtQuick 2.15
import QtQuick.Controls 2.15
import QtWebSockets 1.15
import "." as Ext

Rectangle {
  color: "lavender"

  Ext.Repl {}
  
  WebSocketServer {
    id: server
    objectName: "server"
    port: 8080
    listen: true

    property int counter: 0
    property var connection

    function send(message) {                                                    // called from CLOG
      connection.sendTextMessage(message)
    }

    function log(dictionary) {
      logModel.append(dictionary)
      view.positionViewAtEnd()
    }

    onClientConnected: {
      connection = webSocket
      webSocket.objectName = ++counter
      log({ message: "[new] " + counter, error: false })
      Lisp.call(webSocket, "clog-connection:server/on-new-connection")          // call CLOG

      webSocket.onTextMessageReceived.connect(function(message) {
        log({ message: message, error: false })
        Lisp.call(webSocket, "clog-connection:server/on-message", message)      // call CLOG
      })

      webSocket.onStatusChanged.connect(function(status) {
        var state
        switch (status) {
          case WebSocket.Closed: state = "close"; break
          case WebSocket.Error:  state = "error"; break
          default: return
        }
        log({ message: "[status] " + state, error: status === WebSocket.Error})
        if (status === WebSocket.Closed) {
          Lisp.call(webSocket, "clog-connection:server/on-close")               // call CLOG
        }
      })
    }

    onErrorStringChanged: {
      log({ message: "[server error] " + errorString, error: true });
    }
  }

  ListView {
    id: view
    anchors.fill: parent
    model: ListModel { id: logModel }
    delegate: Text {
      font.pixelSize: 14
      color: error ? "red" : "#111"
      text: message
    }
  }
}
