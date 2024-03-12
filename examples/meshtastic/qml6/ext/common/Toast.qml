import QtQuick

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
  radius: Math.min(25, height / 2)
  opacity: 0
  visible: false

  function message(text, seconds) { // called from Lisp
    pause.duration = 1000 * ((seconds === 0) ? (24 * 60 * 60) : seconds)
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

    MouseArea {
      anchors.fill: parent
      onClicked: toast.visible = false
    }
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
      id: pause
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
