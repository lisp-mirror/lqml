import QtQuick 2.15

RotationAnimation {
  onRunningChanged: Lisp.call("qsoko:animation-change", running)
}

