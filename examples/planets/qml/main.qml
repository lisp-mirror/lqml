import QtQuick 2.15

Item {
  id: main
  objectName: "main"
  width: 300
  height: 500

  Rectangle {
    anchors.fill: parent
    color: "#101010"
  }

  ListView {
    id: view
    objectName: "view"
    anchors.fill: parent
    delegate: planetInfo
    model: planets
  }

  ListModel {
    id: planets
    objectName: "planets"

    // example of inline item
    //ListElement { name: "Earth"; shape: "img/earth.png"; map: "img/earth-map.jpg"; info: "..." }

    function addPlanet(planet) { append(planet) }
  }

  property int itemHeight: 44

  Component {
    id: planetInfo

    Item {
      id: wrapper
      width: view.width
      height: itemHeight

      Rectangle {
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        height: itemHeight
        color: "#303060"
        border.color: Qt.lighter(color, 1.2)

        Text {
          x: 15
          anchors.verticalCenter: parent.verticalCenter
          anchors.leftMargin: 4
          font.pixelSize: parent.height - 22
          color: "#f0f0f0"
          text: model.name // see Lisp keyword name
        }
      }

      Rectangle {
        id: image
        width: itemHeight - 4
        height: width
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.rightMargin: 2
        anchors.topMargin: 2
        color: "#101010"

        Column {
          id: imageColumn
          anchors.fill: parent

          Image {
            id: shapeImage
            height: parent.height - mapImage.height
            width: parent.width
            fillMode: Image.PreserveAspectFit
            source: model.shape // see Lisp keyword name
          }

          Image {
            id: mapImage
            width: parent.width
            height: 0
            fillMode: Image.PreserveAspectFit
            source: model.map // see Lisp keyword name
          }
        }
      }

      MouseArea {
        anchors.fill: parent
        onClicked: parent.state = "expanded"
      }

      Item {
        id: infoView
        anchors.top: image.bottom
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        opacity: 0

        Rectangle {
          anchors.fill: parent
          color: "#303060"
          border.color: "#101010"
          border.width: 1

          Flickable {
            id: flick
            anchors.fill: parent
            anchors.margins: 4
            contentWidth: edit.paintedWidth
            contentHeight: edit.paintedHeight
            clip: true

            function ensureVisible(r) {
              if (contentX >= r.x)
                contentX = r.x;
              else if (contentX+width <= r.x + r.width)
                contentX = r.x + r.width-width;
              if (contentY >= r.y)
                contentY = r.y;
              else if (contentY+height <= r.y + r.height)
                contentY = r.y + r.height-height;
            }

            TextEdit {
              id: edit
              width: flick.width
              color: "#f0f0f0"
              font.pixelSize: 16
              readOnly: true
              focus: true
              wrapMode: TextEdit.Wrap
              onCursorRectangleChanged: flick.ensureVisible(cursorRectangle)
              text: model.info // see Lisp keyword name
            }
          }
        }
      }

      Rectangle {
        id: closeButton
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.rightMargin: 2
        anchors.topMargin: 2
        width: itemHeight - 4
        height: width
        color: "transparent"
        border.color: "#f0f0f0"
        opacity: 0

        Text {
          anchors.centerIn: parent
          color: "#f0f0f0"
          font.bold: true
          text: "X"
        }

        MouseArea {
          anchors.fill: parent
          onClicked: wrapper.state = ""
        }
      }

      states: [
        State {
          name: "expanded"

          PropertyChanges { target: wrapper; height: view.height }
          PropertyChanges { target: image; width: view.width; height: view.height * 2/3; anchors.rightMargin: 0; anchors.topMargin: itemHeight }
          PropertyChanges { target: mapImage; height: view.height * 1/3 }
          PropertyChanges { target: infoView; opacity: 1 }
          PropertyChanges { target: closeButton; opacity: 1 }
          PropertyChanges { target: wrapper.ListView.view; contentY: wrapper.y; interactive: false }
        }
      ]

      transitions: [
        Transition {
          NumberAnimation {
            duration: 250
            properties: "height,width,anchors.rightMargin,anchors.topMargin,opacity,contentY"
          }
        }
      ]
    }
  }
}
