import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Window 2.15
import 'debug/' as Dbg

StackView {
  id: main
  objectName: "main"
  width: 800
  height: 600
  initialItem: mainRect

  // show/hide dialogs

  function pushDialog(name)  {
    switch (name) {
      case "debug": main.push(dialogDebug); break
    }
  }

  function popDialog() { main.pop() }

  // fonts (must stay here, before using them below)

  FontLoader { id: fontHack;     source: "fonts/Hack-Regular.ttf" }
  FontLoader { id: fontHackBold; source: "fonts/Hack-Bold.ttf" }

  // items

  Rectangle {
    id: mainRect
    color: "orange"

    Button {
      id: button
      anchors.centerIn: parent
      text: "Click (or wait 10 seconds)"

      onClicked: Lisp.call("app:crash?")
    }
  }

  // dialogs

  Dbg.DebugDialog { id: dialogDebug }
}
