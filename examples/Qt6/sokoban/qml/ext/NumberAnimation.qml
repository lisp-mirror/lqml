import QtQuick

NumberAnimation {
  onRunningChanged: Lisp.call("qsoko:animation-change", running)
}

