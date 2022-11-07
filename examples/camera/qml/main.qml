import QtQuick 2.15
import QtQuick.Controls 2.15
import QtMultimedia 5.15

Rectangle {
  width: 640
  height: 360
  color: "black"

  Camera {
    id: camera
    objectName: "camera"

    imageCapture {
      onImageSaved: {
        imagePaths.append({"path": "file://" + path})
        listView.positionViewAtEnd()
        Lisp.call("camera:create-index.html", path)
      }
    }
  }

  VideoOutput {
    objectName: "output"
    source: camera
    anchors.fill: parent
    anchors.bottomMargin: listView.height + 10
    focus: visible // to receive focus and capture key events when visible
  }

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

      onClicked: camera.imageCapture.capture()
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

