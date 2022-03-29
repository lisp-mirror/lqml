import QtQuick 2.15

SequentialAnimation {
  onRunningChanged: Lisp.call("qsoko:animation-change", running)
}

