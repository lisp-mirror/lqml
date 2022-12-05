import QtQuick 2.15
import QtQuick.Controls 2.15

Rectangle {
  id: main
  anchors.horizontalCenter: parent.horizontalCenter
  y: 4
  width: canvas.width
  height: column.height
  color: "lightyellow"
  border.width: 1
  border.color: "black"
  radius: 10
  opacity: 0.7

  Column {
    id: column
    width: parent.width
    spacing: 5
    padding: 5

    Row {
      anchors.horizontalCenter: parent.horizontalCenter
      spacing: 5

      Button { // reload
        width: height
        text: "\uf021"
        font.family: fontAwesome.name
        font.pixelSize: 20
        onClicked: Lisp.call("qml:reload-qml-file")
      }
      Button { // minimize/maximize
        width: height
        text: canvas.visible ? "\uf2d1" : "\uf2d0"
        font.family: fontAwesome.name
        font.pixelSize: 20
        onClicked: canvas.visible = !canvas.visible
      }
      Button { // close
        width: height
        text: "\uf00d"
        font.family: fontAwesome.name
        font.pixelSize: 20
        onClicked: main.visible = false
      }
    }

    Canvas {
      id: canvas
      objectName: "canvas"
      anchors.horizontalCenter: parent.horizontalCenter
      width: 195
      height: width

      property var ctx

      // functions to be called from Lisp

      function begin(color, width) {
        ctx.beginPath()
        ctx.strokeStyle = color
        ctx.lineWidth = width
      }

      function end() {
        ctx.stroke()
      }

      function line(x1, y1, x2, y2) {
        ctx.moveTo(x1, y1)
        ctx.lineTo(x2, y2)
      }

      function moveTo(x, y) {
        ctx.moveTo(x, y)
      }

      function lineTo(x, y) {
        ctx.lineTo(x, y)
      }

      onPaint: {
        ctx = getContext("2d")
        ctx.reset()
        ctx.translate(0, height / 2)

        Lisp.call("qml-user:paint")
      }
    }
  }
}
