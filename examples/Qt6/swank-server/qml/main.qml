import QtQuick
import QtQuick.Controls
import "ext/" as Ext

Item {
  width: 300
  height: 500
  objectName: "main"

  Ext.Repl {}

  FontLoader { id: fontIcons;    source: "fonts/fontawesome-webfont.ttf" }
  FontLoader { id: fontHack;     source: "fonts/Hack-Regular.ttf" }
  FontLoader { id: fontHackBold; source: "fonts/Hack-Bold.ttf" }
}
