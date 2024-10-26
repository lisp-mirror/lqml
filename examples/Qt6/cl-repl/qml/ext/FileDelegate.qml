import QtQuick

Rectangle {
  width: folderView.width
  height: 48
  color: (index === folderView.currentIndex) ? "lightskyblue" : folderView.colors[index & 1]

  Row {
    anchors.fill: parent

    Text {
      id: icon
      width: 38
      anchors.verticalCenter: parent.verticalCenter
      font.family: fontAwesome.name
      font.pixelSize: 24
      text: fileIsDir ? " \uf115" : " \uf016"
    }
    Text {
      width: 3/4 * folderView.width - icon.width
      anchors.verticalCenter: parent.verticalCenter
      font.pixelSize: 18
      text: fileName
    }
    Text {
      width: 1/4 * folderView.width - 4
      anchors.verticalCenter: parent.verticalCenter
      horizontalAlignment: Text.AlignRight
      font.pixelSize: 18
      text: fileIsDir ? "" : Lisp.call("cl:format", null, "~:D", fileSize)
    }
  }

  MouseArea {
    anchors.fill: parent

    onClicked: {
      // highlight selected
      folderView.currentIndex = index
      Lisp.call("qml:qsleep", 0.1)
      folderView.currentIndex = -1

      if (fileBrowser.editMode) {
        path.text = filePath
        fileBrowser.editFrom = filePath
        path.forceActiveFocus()
        var start = filePath.lastIndexOf("/") + 1
        var end = filePath.lastIndexOf(".")
        if (end > start) {
          path.cursorPosition = start
          path.moveCursorSelection(end, TextInput.SelectCharacters)
        }
      } else {
        if (fileIsDir) {
          Lisp.call("dialogs:set-file-browser-path", filePath)
        }
        else {
          fileBrowser.visible = false
          Lisp.call("dialogs:set-file-name", filePath)
        }
      }
    }
  }
}
