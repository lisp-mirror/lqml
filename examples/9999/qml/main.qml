import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Window 2.15
import Lisp 1.0

Rectangle {
  id: main
  width: 200
  height: 300 + input.height
  color: "lavender"

  TextField {
    id: input
    objectName: "input"
    width: parent.width
    horizontalAlignment: Qt.AlignHCenter
    text: "0000"
    inputMask: "9999"
    inputMethodHints: Qt.ImhDigitsOnly
    focus: true

    onTextChanged: Lisp.call("app:draw-number", Number(text))
  }

  Canvas {
    id: canvas
    objectName: "canvas"
    y: input.height
    width: parent.width
    height: {
      var h = Qt.inputMethod.keyboardRectangle.y
      var f = (Qt.platform.os === "android") ? Screen.devicePixelRatio : 1
      h = (h === 0) ? main.height : h / f
      return (h - input.height)
    }

    property var ctx

    // functions to be called from Lisp

    function begin(color, width) {
      ctx.beginPath()
      ctx.strokeStyle = color
      ctx.lineWidth = width
      ctx.lineCap = "round"
    }

    function end() {
      ctx.stroke()
    }

    function drawLine(x1, y1, x2, y2) {
      ctx.moveTo(x1, y1)
      ctx.lineTo(x2, y2)
    }

    onPaint: {
      ctx = getContext("2d")
      ctx.reset()
      ctx.translate(canvas.width / 2, canvas.height / 2)
      var s = height / 340
      ctx.scale(s, s)

      Lisp.call("app:paint")

      ctx.stroke()
    }
  }
}
