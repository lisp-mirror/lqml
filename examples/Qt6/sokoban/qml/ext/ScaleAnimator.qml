import QtQuick

ScaleAnimator {
  onRunningChanged: Lisp.call("qsoko:animation-change", running)
}
