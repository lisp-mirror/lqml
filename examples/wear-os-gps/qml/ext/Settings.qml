import QtQuick 2.15
import QtQuick.Controls 2.15

Rectangle {

  Column {
    anchors.centerIn: parent

    Text {
      anchors.horizontalCenter: parent.horizontalCenter
      text: "max speed"
      font.pixelSize: 16
      font.weight: Font.DemiBold
    }

    Tumbler {
      id: maxSpeed
      objectName: "max_speed"
      anchors.horizontalCenter: parent.horizontalCenter
      height: 80
      model: [10, 20, 30, 40, 50]
      visibleItemCount: 3

      property int value: model[0]

      onCurrentIndexChanged: { value = model[currentIndex] }
      onValueChanged:        { currentIndex = model.indexOf(value) }

      delegate: Text {
        text: modelData
        font.pixelSize: 22
        font.bold: true
        horizontalAlignment: Text.AlignHCenter
        opacity: 0.4 + Math.max(0, 1 - Math.abs(Tumbler.displacement)) * 0.6

        Component.onCompleted: {
          maxSpeed.width = paintedWidth * 1.1 // 1.1 for iOS font (not fixed width)
        }
      }
    }

    Switch {
      objectName: "always_on"
      text: "always on"
      font.pixelSize: 16
      font.weight: Font.DemiBold
      checked: true

      onCheckedChanged: Lisp.call("gps:always-on-changed", checked)
    }
  }
}
