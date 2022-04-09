import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Window 2.15
import 'ext/' as Ext
import 'ext/dialogs' as Dlg

StackView {
  id: main
  objectName: "main"
  width: Screen.desktopAvailableWidth
  height: Screen.desktopAvailableHeight
  initialItem: mainRect
  Screen.orientationUpdateMask: Qt.LandscapeOrientation | Qt.PortraitOrientation | Qt.InvertedLandscapeOrientation

  property bool small: (Math.max(width, height) < 1000)
  property bool skipEnsureVisible: false

  function availableHeight() {
    var h = Math.round(Qt.inputMethod.keyboardRectangle.y /
                       ((Qt.platform.os === "android") ? Screen.devicePixelRatio : 1))
    return (h === 0) ? main.height : h
  }

  function halfHeight()       { return (availableHeight() - rectCommand.height) / 2 }
  function isLandscape()      { return (Screen.primaryOrientation === Qt.LandscapeOrientation) }
  function keyboardVisible()  { return Qt.inputMethod.visible }
  function showKeyboard(show) { show ? Qt.inputMethod.show() : Qt.inputMethod.hide() }

  // show/hide dialogs

  function pushDialog(name)  {
    switch (name) {
      case "file":  main.push(dialogFile);  break;
      case "debug": main.push(dialogDebug); break;
      case "query": main.push(dialogQuery); break;
    }
  }

  function popDialog() { main.pop() }

  Screen.onOrientationChanged: {
    Lisp.call("editor:orientation-changed", Screen.orientation)
  }

  Keys.onPressed: {
    if(event.key === Qt.Key_Back) {
      event.accepted = true
      Lisp.call("editor:back-pressed")
    }
  }

  // custom transition animations

  pushEnter: Transition {
    ParallelAnimation {
      OpacityAnimator {
        from: 0
        to: 1
        easing.type: Easing.OutQuart
        duration: 300
      }
      XAnimator {
        from: width / 3
        to: 0
        easing.type: Easing.OutQuart
        duration: 300
      }
    }
  }

  pushExit: Transition {
    OpacityAnimator {
      from: 1
      to: 0
      duration: 300
    }
  }

  popEnter: Transition {
    OpacityAnimator {
      from: 0
      to: 1
      duration: 300
    }
  }

  popExit: Transition {
    ParallelAnimation {
      OpacityAnimator {
        from: 1
        to: 0
        easing.type: Easing.InQuart
        duration: 300
      }
      XAnimator {
        from: 0
        to: width / 3
        easing.type: Easing.InQuart
        duration: 300
      }
    }
  }

  // delay timer

  Timer {
    id: timer
  }

  function delay(milliseconds, callback) {
    timer.interval = milliseconds
    timer.triggered.connect(callback)
    timer.start()
  }

  function later(callback) {
    delay(50, callback)
  }

  // fonts (must stay here, before using them below)

  FontLoader { id: fontHack;     source: "fonts/Hack-Regular.ttf" }        // code
  FontLoader { id: fontHackBold; source: "fonts/Hack-Bold.ttf" }
  FontLoader { id: fontAwesome;  source: "fonts/fontawesome-webfont.ttf" } // icons

  // items

  Rectangle {
    id: mainRect

    Rectangle {
      id: rectEdit
      objectName: "rect_edit"
      width: main.width
      height: main.halfHeight()

      Ext.Flickable {
        id: flickEdit
        objectName: "flick_edit"
        anchors.fill: parent
        contentWidth: edit.paintedWidth
        contentHeight: edit.paintedHeight

        TextEdit {
          id: edit
          objectName: "edit"
          width: flickEdit.width
          height: flickEdit.height
          leftPadding: 2
          font.family: "Hack"
          font.pixelSize: 18
          selectionColor: "firebrick"
          inputMethodHints: Qt.ImhNoAutoUppercase | Qt.ImhNoPredictiveText | Qt.ImhNoTextHandles | Qt.ImhNoEditMenu
          cursorDelegate: cursor

          Keys.onTabPressed: command.forceActiveFocus()

          onCursorRectangleChanged: flickEdit.ensureVisible(cursorRectangle)

          Component.onCompleted: later(function() {
            Lisp.call(textDocument, "editor:set-text-document", objectName)
          })

          // for external keyboard
          Shortcut {
            sequence: "Ctrl+E" // E for Expression
            onActivated: Lisp.call("editor:select-expression")
          }
          Shortcut {
            sequence: "Ctrl+L" // L for Lambda
            onActivated: Lisp.call("editor:eval-single-expression")
          }

          MouseArea {
            width: Math.max(rectEdit.width, edit.paintedWidth)
            height: Math.max(rectEdit.height, edit.paintedHeight)

            onPressed: {
              // seems necessary to consistently move cursor by tapping
              edit.forceActiveFocus()
              edit.cursorPosition = edit.positionAt(mouse.x, mouse.y)
              Qt.inputMethod.show() // needed for edge case (since we have 2 input fields)
              Lisp.call("editor:set-focus-editor", edit.objectName)
            }

            onPressAndHold: Lisp.call("editor:copy-paste", edit.cursorPosition)
          }
        }

        Component {
          id: cursor

          Rectangle {
            width: 2
            color: "blue"
            visible: parent.activeFocus

            SequentialAnimation on opacity {
              running: true
              loops: Animation.Infinite

              NumberAnimation { to: 0; duration: 500; easing.type: "OutQuad" }
              NumberAnimation { to: 1; duration: 500; easing.type: "InQuad" }
            }
          }
        }
      }
    }

    Rectangle {
      id: buttonsTop
      objectName: "buttons_top"
      y: -height // hidden
      width: rowButtonsTop.width
      height: rowButtonsTop.height
      anchors.horizontalCenter: parent.horizontalCenter
      opacity: 0.7

      Row {
        id: rowButtonsTop
        padding: 4
        spacing: 6

        Ext.MenuButton {
          objectName: "undo"
          text: "\uf0e2"
          enabled: edit.canUndo
        }
        Ext.MenuButton {
          objectName: "redo"
          text: "\uf01e"
          enabled: edit.canRedo
        }
        Ext.MenuButton {
          objectName: "font_smaller"
          text: "\uf010"
          font.pixelSize: main.small ? 10 : 15
        }
        Ext.MenuButton {
          objectName: "font_bigger"
          text: "\uf00e"
          font.pixelSize: main.small ? 16 : 25
        }
      }
    }

    Rectangle {
      id: rectCommand
      objectName: "rect_command"
      y: flickEdit.height
      width: parent.width
      height: command.font.pixelSize + 11
      border.width: 2
      border.color: command.focus ? "#0066ff" : "lightgray"

      Ext.Flickable {
        id: flickCommand
        objectName: "flick_command"
        anchors.fill: parent
        contentWidth: command.paintedWidth
        contentHeight: command.paintedHeight

        TextEdit {
          id: command
          objectName: "command"
          width: flickCommand.width
          height: flickCommand.height
          padding: 4
          font.family: "Hack"
          font.pixelSize: 18
          selectionColor: "firebrick"
          inputMethodHints: Qt.ImhNoAutoUppercase | Qt.ImhNoPredictiveText | Qt.ImhNoTextHandles | Qt.ImhNoEditMenu
          cursorDelegate: cursor
          focus: true

          Keys.onUpPressed:   Lisp.call("editor:history-move", "back")
          Keys.onDownPressed: Lisp.call("editor:history-move", "forward")
          Keys.onTabPressed:  edit.forceActiveFocus()

          onCursorRectangleChanged: flickCommand.ensureVisible(cursorRectangle)

          Component.onCompleted: later(function() {
            Lisp.call(textDocument, "editor:set-text-document", objectName)
          })

          MouseArea {
            width: Math.max(rectCommand.width, command.paintedWidth)
            height: Math.max(rectCommand.height, command.paintedHeight)

            onPressed: {
              // seems necessary to consistently move cursor by tapping
              command.forceActiveFocus()
              command.cursorPosition = command.positionAt(mouse.x, mouse.y)
              Qt.inputMethod.show() // needed for edge case (since we have 2 input fields)
              Lisp.call("editor:set-focus-editor", command.objectName)
              Lisp.call("editor:ensure-output-visible")
            }

            onPressAndHold: Lisp.call("editor:copy-paste", command.cursorPosition)
          }
        }
      }
    }
 
    ProgressBar {
      objectName: "progress"
      anchors.top: rectCommand.bottom
      width: main.width
      z: 1
      indeterminate: true
      enabled: visible
      visible: false
    }

    Rectangle {
      id: rectOutput
      objectName: "rect_output"
      y: flickEdit.height + rectCommand.height
      width: main.width
      height: main.halfHeight()

      ListView {
        id: output
        objectName: "output"
        anchors.fill: parent
        contentWidth: parent.width * 5
        clip: true
        model: outputModel
        flickableDirection: Flickable.HorizontalAndVerticalFlick

        property string fontFamily: "Hack"
        property int fontSize: 18

        delegate: Column {
          Rectangle {
            width: output.contentWidth
            height: mLine ? 2 : 0
            color: "#c0c0ff"
          }

          Text {
            x: 2
            padding: 2
            textFormat: Text.PlainText
            font.family: output.fontFamily
            font.pixelSize: output.fontSize
            text: mRichText ? "" : mText
            color: mColor
            font.bold: mBold
            visible: !mRichText
          }

          Text {
            x: 2
            padding: 2
            textFormat: Text.RichText
            font.family: output.fontFamily
            font.pixelSize: output.fontSize
            text: mRichText ? mText : ""
            color: mColor
            font.bold: mBold
            visible: mRichText

            MouseArea {
              width: parent.paintedWidth
              height: parent.paintedHeight

              onPressed: {
                // custom link handling, since 'onLinkActivated' does not work within a Flickable
                var link = parent.linkAt(mouse.x, mouse.y)
                if(link.length) {
                  Qt.openUrlExternally(link)
                }
              }
            }
          }
        }

        onFlickStarted: forceActiveFocus()

        Component.onCompleted: later(function () {
          Lisp.call("editor:delayed-ini")
        })
      }

      ListModel {
        id: outputModel
        objectName: "output_model"

        function appendOutput(text) {
          append(text)
          output.contentX = 0
          output.positionViewAtEnd()
        }
      }
    }

    Ext.MenuButton {
      id: showMenu
      objectName: "show_menu"
      x: parent.width - width - 4
      y: 4
      opacity: 0.7
      text: "\uf142"

      onClicked: {
        showButtonsTop.start()
        showButtonsRight.start()
        menuTimer.start()
      }
    }

    Timer {
      id: menuTimer
      objectName: "menu_timer"
      interval: 3000
      onTriggered: {
        if (buttonsTop.y === 0) {
          hideButtonsTop.start()
          hideButtonsRight.start()
        }
      }
    }

    Rectangle {
      id: buttonsRight
      objectName: "buttons_right"
      x: -width // hidden
      width: colButtonsRight.width
      height: colButtonsRight.height

      Column {
        id: colButtonsRight
        padding: 4
        spacing: 6

        Ext.Button {
          objectName: "clear"
          text: "\uf014"
        }
        Ext.Button {
          objectName: "open_file"
          text: "\uf115"
        }
        Ext.Button {
          objectName: "save_file"
          text: "\uf0c7"
        }
        Ext.Button {
          objectName: "eval"
          text: "\u03bb" // lambda
        }
      }
    }

    Rectangle {
      id: buttonsBottom
      width: rowButtonsBottom.width
      height: rowButtonsBottom.height
      anchors.horizontalCenter: parent.horizontalCenter
      anchors.top: rectOutput.top
      opacity: 0.7
      visible: command.activeFocus

      Row {
        id: rowButtonsBottom
        padding: 4
        spacing: 6

        Ext.MenuButton {
          objectName: "history_back"
          text: "\uf100"
        }
        Ext.MenuButton {
          objectName: "history_forward"
          text: "\uf101"
        }
      }
    }

    // animations for showing/hiding editor menu buttons

    NumberAnimation {
      id: showButtonsTop
      objectName: "show_buttons_top"
      target: buttonsTop
      property: "y"
      from: -buttonsTop.height
      to: 0
      duration: 500
      easing.type: Easing.OutExpo
    }

    NumberAnimation {
      id: showButtonsRight
      objectName: "show_buttons_right"
      target: buttonsRight
      property: "x"
      from: buttonsRight.parent.width
      to: buttonsRight.parent.width - buttonsRight.width
      duration: 500
      easing.type: Easing.OutExpo
    }

    NumberAnimation {
      id: hideButtonsTop
      target: buttonsTop
      property: "y"
      from: 0
      to: -buttonsTop.height
      duration: 500
      easing.type: Easing.InExpo
    }

    NumberAnimation {
      id: hideButtonsRight
      target: buttonsRight
      property: "x"
      from: buttonsRight.parent.width - buttonsRight.width
      to: buttonsRight.parent.width
      duration: 500
      easing.type: Easing.InExpo
    }

    // paren buttons (above keyboard)

    Rectangle {
      objectName: "rect_paren_buttons"
      width: rowParens.width
      height: rowParens.height
      anchors.horizontalCenter: rectOutput.horizontalCenter
      anchors.bottom: rectOutput.bottom
      color: "transparent"
      visible: Qt.inputMethod.visible

      Row {
        id: rowParens
        padding: 5
        spacing: 5

        Ext.ParenButton {
          source: "img/paren-open.png"
          onClicked: Lisp.call("editor:insert", "(")
        }
        Ext.ParenButton {
          source: "img/paren-close.png"
          onClicked:      Lisp.call("editor:insert", ")")
          onPressAndHold: Lisp.call("editor:close-all-parens")
        }
      }
    }

    // arrow buttons (cursor movement)

    Rectangle {
      id: rectArrows
      objectName: "rect_arrows"
      width: arrows.width + 20
      height: width
      anchors.right: rectOutput.right
      anchors.bottom: rectOutput.bottom
      color: "transparent"
      visible: Qt.inputMethod.visible

      MouseArea {
        anchors.fill: parent
        onPressed: Lisp.call("editor:ensure-focus")
      }

      Item {
        id: arrows
        width: up.width * 3
        height: width
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.verticalCenter: parent.verticalCenter

        Ext.ArrowButton {
          id: up
          objectName: "up"
          text: "\uf139"
          anchors.horizontalCenter: parent.horizontalCenter
        }

        Ext.ArrowButton {
          objectName: "left"
          text: "\uf137"
          anchors.verticalCenter: parent.verticalCenter
        }

        Ext.ArrowButton {
          objectName: "right"
          text: "\uf138"
          anchors.verticalCenter: parent.verticalCenter
          anchors.right: parent.right
        }

        Ext.ArrowButton {
          objectName: "down"
          text: "\uf13a"
          anchors.horizontalCenter: parent.horizontalCenter
          anchors.bottom: parent.bottom
        }
      }
    }
  }

  // custom font loader

  function loadFont(file) {
    var font = Qt.createQmlObject("import QtQuick 2.10; FontLoader { source: '" + file + "' }", main)
    return font.name
  }

  // not visible dialog / menu instances

  Ext.FileBrowser { id: dialogFile;  opacity: 0 }
  Ext.QueryDialog { id: dialogQuery; opacity: 0 }
  Ext.DebugDialog { id: dialogDebug; opacity: 0 }

  Ext.ClipboardMenu {}

  // modal dialogs

  Dlg.Dialogs {}
}
