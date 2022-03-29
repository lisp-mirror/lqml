import QtQuick 2.15

NumberAnimation {
  onRunningChanged: Lisp.call("qsoko:animation-change", running)
}

