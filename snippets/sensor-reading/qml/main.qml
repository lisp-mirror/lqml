import QtQuick 2.15
import QtSensors 5.15

Rectangle {
  width: 300
  height: 500

  Accelerometer {
    id: accel
    active: true
    accelerationMode: Accelerometer.User // exclude gravity
  }

  Timer {
    id: timer
    interval: 50
    repeat: true
    running: true

    onTriggered: Lisp.call("qml-user:accel-changed",
                           accel.reading.x, accel.reading.y, accel.reading.z)
  }
}
