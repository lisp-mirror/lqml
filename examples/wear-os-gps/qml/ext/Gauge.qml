import QtQuick 2.15

Rectangle {
  id: gauge
  objectName: "gauge"
  anchors.centerIn: parent
  width: Math.min(parent.height, parent.width)
  height: width
  color: "black"

  // main properties
  property double value:        0
  property double maximumValue: 10
  property double limit:        8/10

  property double r: canvas.width / 2 // main radius
  property double f: width / 400      // size factor

  onLimitChanged: canvas.requestPaint()

  // ticks

  Canvas {
    id: canvas
    z: 1
    objectName: "gauge_canvas"
    anchors.fill: parent

    property var ctx

    function rotate(angle)            { ctx.rotate(angle) }
    function beginPath()              { ctx.beginPath() }
    function arc(x, y, r, start, end) { ctx.arc(x, y, r, start, end) }
    function save()                   { ctx.save() }
    function restore()                { ctx.restore() }
    function stroke()                 { ctx.stroke() }

    function setStyle(color, width) {
      ctx.strokeStyle = color
      ctx.lineWidth = width
    }

    function drawLine(x1, y1, x2, y2) {
      ctx.moveTo(x1, y1)
      ctx.lineTo(x2, y2)
    }

    onPaint: {
      ctx = getContext("2d")
      ctx.reset()
      ctx.translate(gauge.r, gauge.r)

      Lisp.call("gauge:paint")
    }

    onWidthChanged:  numbersLoader.reload()
    onHeightChanged: numbersLoader.reload()
  }

  // numbers (needs delayed loading on startup, hence 'Component' and 'Loader')

  Component {
    id: numbers

    Repeater {
      model: 11

      Text {
        x: Lisp.call("gauge:number-pos", "x", index) - paintedWidth / 2
        y: Lisp.call("gauge:number-pos", "y", index) - paintedHeight / 2
        color: "white"
        text: Math.round(gauge.maximumValue / 10 * index)
        font.pixelSize: 28 * gauge.f
        font.bold: true
      }
    }
  }

  Loader {
    id: numbersLoader
    objectName: "gauge_numbers_loader"
    active: false
    sourceComponent: numbers

    function reload() {
      if (active) {
        active = false
        active = true
      }
    }
  }

  // needle

  Item {
    x: gauge.r
    y: x
    rotation: 180 / gauge.maximumValue
              * Math.max(0, Math.min(gauge.value, gauge.maximumValue))
              + 90

    Behavior on rotation {
      NumberAnimation { duration: 1000; easing.type: Easing.InOutCubic }
    }

    Rectangle {
      x: -width / 2
      y: x
      width: 28 * gauge.f
      height: width
      radius: width / 2
      color: "white"
    }

    Rectangle {
      x: -width / 2
      y: -30 * gauge.f
      width: 14 * gauge.f
      height: gauge.r - 28 * gauge.f
      radius: width / 2
      color: "white"
    }
  }
}
