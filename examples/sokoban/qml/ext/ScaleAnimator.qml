import QtQuick 2.15

ScaleAnimator {
  onRunningChanged: Lisp.call("qsoko:animation-change", running)
}
