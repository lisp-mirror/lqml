import QtQuick 2.15
import QtQuick.Controls 2.15
import "ext/" as Ext
import Lisp 1.0

Item {
  width: 300
  height: 500

  Ext.Repl {}

  FontLoader { id: fontIcons;    source: "fonts/fontawesome-webfont.ttf" }
  FontLoader { id: fontHack;     source: "fonts/Hack-Regular.ttf" }
  FontLoader { id: fontHackBold; source: "fonts/Hack-Bold.ttf" }
}
