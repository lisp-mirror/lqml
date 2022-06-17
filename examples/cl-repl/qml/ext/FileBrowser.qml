import QtQuick 2.15
import QtQuick.Controls 2.15
import Qt.labs.folderlistmodel 2.15
import "." as Ext

Rectangle {
  id: fileBrowser
  objectName: "file_browser"
  visible: false

  property bool editMode: false
  property string editFrom

  function urlToString(url) {
    var cut = (Qt.platform.os === "windows") ? "file:/" : "file://"
    return url.toString().substring(cut.length)
  }

  Rectangle {
    id: header
    width: fileBrowser.width
    height: headerColumn.height
    z: 2
    color: "#f0f0f0"

    Column {
      id: headerColumn

      Ext.MenuBack {
        id: menuBack

        Row {
          id: buttonRow
          spacing: 4
          anchors.horizontalCenter: parent.horizontalCenter

          // one directory up
          Ext.FileButton {
            text: "\uf062"
            onClicked: Lisp.call("dialogs:set-file-browser-path",
                                 urlToString(folderModel.parentFolder))
          }

          // documents
          Ext.FileButton {
            text: "\uf0f6"
            onClicked: Lisp.call("dialogs:set-file-browser-path", ":data")
          }

          // home
          Ext.FileButton {
            text: "\uf015"
            onClicked: Lisp.call("dialogs:set-file-browser-path", ":home")
          }
        }
      }

      Ext.TextField {
        id: path
        objectName: "path"
        width: fileBrowser.width
        inputMethodHints: Qt.ImhNoAutoUppercase | Qt.ImhNoPredictiveText
        text: urlToString(folderModel.folder)

        onFocusChanged: if(focus) { cursorPosition = length }

        onAccepted: {
          if(fileBrowser.editMode) {
            Lisp.call("dialogs:rename-file*", fileBrowser.editFrom, path.text)
            fileBrowser.editMode = false
          } else {
            Lisp.call("dialogs:set-file-name", text)
          }
        }
      }
    }

    // edit mode
    Ext.FileButton {
      id: fileEdit
      objectName: "file_edit"
      anchors.right: parent.right
      contentItem: Text {
        id: editButton
        horizontalAlignment: Text.AlignHCenter
        verticalAlignment: Text.AlignVCenter
        text: "\uf044"
        color: fileBrowser.editMode ? "red" : "#007aff"
        font.family: fontAwesome.name
        font.pixelSize: 24
      }

      onClicked: fileBrowser.editMode = !fileBrowser.editMode
    }
  }

  ListView {
    id: folderView
    objectName: "folder_view"
    y: header.height
    width: parent.width
    height: parent.height - y
    delegate: Ext.FileDelegate {}
    currentIndex: -1 // no initial highlight
    footerPositioning: ListView.OverlayHeader

    property var colors: ["white", "#f0f0f0"]

    model: FolderListModel {
      id: folderModel
      objectName: "folder_model"
      showDirsFirst: true
      showHidden: true
      nameFilters: ["*.lisp", "*.lsp", "*.qml", "*.asd", "*.exp", "*.sexp",
                    "*.fas", "*.fasb", "*.fasc", ".eclrc", ".repl-history"]
    }

    Row {
      y: main.small ? 7 : 10
      anchors.horizontalCenter: parent.horizontalCenter
      spacing: 20
      visible: path.focus

      // cursor back
      Ext.ArrowButton {
        opacity: 0.15
        text: "\uf137"

        onPressed:      path.cursorPosition--
        onPressAndHold: path.cursorPosition = 0
      }

      // cursor forward
      Ext.ArrowButton {
        opacity: 0.15
        text: "\uf138"

        onPressed:      path.cursorPosition++
        onPressAndHold: path.cursorPosition = path.length
      }
    }

    footer: Rectangle {
      width: fileBrowser.width
      height: itemCount.height + 4
      z: 2
      color: "lightgray"
      border.width: 1
      border.color: "gray"

      Row {
        anchors.fill: parent

        Text {
          id: itemCount
          text: Lisp.call("cl:format", null, " ~D item~P", folderModel.count, folderModel.count)
          anchors.verticalCenter: parent.verticalCenter
        }
      }
    }
  }
}
