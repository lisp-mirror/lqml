import QtQuick 2.15
import QtQuick.Controls 2.15
import Lisp 1.0

Rectangle {
  width: 220
  height: 320 + input.height
  color: "lavender"

  Canvas {
    id: canvas
    objectName: "canvas"
    width: 220
    height: 320

    property var painter
      
    function drawLine(x1, y1, x2, y2) {
      painter.moveTo(x1, y1)
      painter.lineTo(x2, y2)
    }

    onPaint: {
      var ctx = getContext("2d")
      painter = ctx
      ctx.reset()
      ctx.strokeStyle = "blue"
      ctx.lineWidth = 10
      ctx.lineCap = "round"
      ctx.translate(110, 160)
            
      Lisp.call("qml-user:paint")
            
      ctx.stroke()
    }
  }

  TextField {
    id: input
    objectName: "input"
    width: parent.width
    anchors.bottom: parent.bottom
    horizontalAlignment: Qt.AlignHCenter
    text: "0000"
    inputMask: "9999"

    onTextChanged: Lisp.call("qml-user:draw-number", Number(text))
  }
}
