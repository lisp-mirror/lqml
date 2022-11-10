import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Window 2.15
import QtMultimedia 5.15

Rectangle {
  width: 640
  height: 360
  color: "black"
  Screen.orientationUpdateMask:
    Qt.LandscapeOrientation | Qt.PortraitOrientation |
    Qt.InvertedLandscapeOrientation | Qt.InvertedPortraitOrientation

  property int rotation: 0 // iOS: saved image will be rotated by this angle

  Camera {
    id: camera
    objectName: "camera"

    imageCapture {
      onImageSaved: {
        Lisp.call("camera:create-index.html", path, rotation)
        imagePaths.append({"path": "file://" + path})
        listView.positionViewAtEnd()
      }
    }
  }

  Audio {
    id: clickSound
    source: "audio/camera.mp3"
  }

  VideoOutput {
    id: videoOutput
    objectName: "output"
    source: camera
    anchors.fill: parent
    anchors.bottomMargin: listView.height + 10
    focus: visible // to receive focus and capture key events when visible
    autoOrientation: true

    Component.onCompleted: updateImageRotation(Screen.orientation)
  }

  // for iOS
  function updateImageRotation(orientation) {
    if (Qt.platform.os === "ios") {
      switch (orientation) {
        case Qt.LandscapeOrientation:         rotation = 0;   break
        case Qt.PortraitOrientation:          rotation = 90;  break
        case Qt.InvertedLandscapeOrientation: rotation = 180; break
        case Qt.InvertedPortraitOrientation:  rotation = -90; break
      }
    }
  }

  Screen.onOrientationChanged: updateImageRotation(Screen.orientation)

  // menu buttons

  Column {
    anchors.right: parent.right
    padding: 10
    spacing: 10

    ComboBox {
      id: cameras
      width: 170
      model: QtMultimedia.availableCameras
      textRole: "displayName"
      valueRole: "deviceId"

      onActivated: camera.deviceId = currentValue
    }

    RoundButton {
      anchors.right: parent.right
      anchors.rightMargin: parent.spacing
      width: 80
      height: width
      text: "Photo"

      onClicked: {
        clickSound.play()
        camera.imageCapture.capture()
      }
    }
  }

  // list of taken images

  ListModel {
    id: imagePaths
  }

  Rectangle {
    anchors.fill: listView
    anchors.topMargin: -listView.spacing
    color: "gray"
  }

  ListView {
    id: listView
    height: parent.height / 5
    anchors.left: parent.left
    anchors.right: parent.right
    anchors.bottom: parent.bottom
    spacing: 10
    orientation: ListView.Horizontal
    model: imagePaths

    delegate: Image {
      required property string path
      height: parent.height
      source: path
      fillMode: Image.PreserveAspectFit
    }
  }
}

