import QtQuick

RotationAnimation {
  onRunningChanged: Lisp.call("qsoko:animation-change", running)
}

