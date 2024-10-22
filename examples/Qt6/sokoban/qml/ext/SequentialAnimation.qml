import QtQuick

SequentialAnimation {
  onRunningChanged: Lisp.call("qsoko:animation-change", running)
}

