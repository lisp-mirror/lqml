import QtQuick 2.15

Rectangle {
  id: toast
  objectName: "toast"
  x: (parent.width - width) / 2
  y: (parent.height - height) / 2
  z: 99
  width: msg.contentWidth + 70
  height: msg.contentHeight + 30
  color: "#303030"
  border.width: 2
  border.color: "white"
  radius: height / 2
  opacity: 0
  visible: false

  function message(text) { // called from Lisp
    toast.visible = true
    msg.text = text
    anim.start()
  }

  Text {
    id: msg
    font.pixelSize: 16
    font.bold: true
    anchors.centerIn: parent
    color: "white"
    wrapMode: Text.WordWrap
    width: toast.parent.width - 2 * toast.radius - 10
    horizontalAlignment: Text.AlignHCenter
    verticalAlignment: Text.AlignVCenter
  }

  SequentialAnimation {
    id: anim
    onFinished: { toast.visible = false }

    OpacityAnimator {
      from: 0
      to: 0.8
      target: toast
      easing.type: Easing.InOutQuart
      duration: 500
    }

    PauseAnimation {
      duration: 3000
    }

    OpacityAnimator {
      from: 0.8
      to: 0
      target: toast
      easing.type: Easing.InOutQuart
      duration: 1500
    }
  }
}
