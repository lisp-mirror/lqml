import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Styles 1.4
import QtQuick.Extras 1.4

Rectangle {
  anchors.fill: parent
  color: "black"

  CircularGauge {
    id: gauge
    objectName: "speed"
    anchors.fill: parent
    maximumValue: 10
    stepSize: 0.1

    Behavior on value {
      NumberAnimation {
        duration: 1000
        easing.type: Easing.InOutCubic
      }
    }

    style: CircularGaugeStyle {
      id: style
      labelStepSize: 1
      tickmarkStepSize: 1
      minorTickmarkCount: 5
      minimumValueAngle: -90
      maximumValueAngle: 90

      property int limit: parent.maximumValue * 8/10
      property string baseColor: "white"
      property string limitColor: "orange"

      function toRad(deg) { return deg * (Math.PI / 180) }

      background: Canvas {
        onPaint: {
          var ctx = getContext("2d")
          ctx.reset()
          ctx.beginPath()
          ctx.strokeStyle = limitColor
          ctx.lineWidth = outerRadius * 0.02
          ctx.arc(outerRadius, outerRadius, outerRadius - ctx.lineWidth / 2,
                  toRad(valueToAngle(limit) - 90), toRad(valueToAngle(gauge.maximumValue) - 90))
          ctx.stroke()
        }
      }

      tickmark: Rectangle {
        implicitWidth: outerRadius * 0.02
        antialiasing: true
        implicitHeight: outerRadius * 0.06
        color: styleData.value >= limit ? limitColor : baseColor
      }

      minorTickmark: Rectangle {
        visible: styleData.value < limit
        implicitWidth: outerRadius * 0.01
        antialiasing: true
        implicitHeight: outerRadius * 0.03
        color: baseColor
      }

      tickmarkLabel: Text {
        font.pixelSize: Math.max(6, outerRadius * 0.15)
        font.bold: true
        text: styleData.value
        color: styleData.value >= limit ? limitColor : baseColor
        antialiasing: true
      }

      needle: Rectangle {
        y: outerRadius * 0.15
        implicitWidth: outerRadius * 0.07
        implicitHeight: outerRadius * 0.85
        radius: implicitWidth / 2
        antialiasing: true
        color: baseColor
      }

      foreground: Rectangle {
        width: outerRadius * 0.15
        height: width
        radius: width / 2
        color: baseColor
        anchors.centerIn: parent
      }
    }
  }
} 
