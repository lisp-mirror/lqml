import QtQuick 2.15

Image {
  Behavior on x { NumberAnimation { duration: 3000; easing.type: Easing.InOutSine } }
  Behavior on y { NumberAnimation { duration: 3000; easing.type: Easing.InOutSine } }
}

