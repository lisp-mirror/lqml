import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Window 2.15
import 'debug/' as Dbg

StackView {
  id: main
  objectName: "main"
  width: 800  // alternatively: Screen.desktopAvailableWidth
  height: 600 // alternatively: Screen.desktopAvailableHeight
  initialItem: mainRect
  Screen.orientationUpdateMask: Qt.LandscapeOrientation | Qt.PortraitOrientation | Qt.InvertedLandscapeOrientation

  // show/hide dialogs

  function pushDialog(name)  {
    switch (name) {
      case "debug": main.push(dialogDebug); break
    }
  }

  function popDialog() { main.pop() }

  // fonts (must stay here, before using them below)

  FontLoader { id: fontHack;     source: "fonts/Hack-Regular.ttf" }        // code
  FontLoader { id: fontHackBold; source: "fonts/Hack-Bold.ttf" }
  FontLoader { id: fontAwesome;  source: "fonts/fontawesome-webfont.ttf" } // icons

  // items

  Rectangle {
    id: mainRect
    color: "lavender"
  }

  // dialogs

  Dbg.DebugDialog { id: dialogDebug }
}
