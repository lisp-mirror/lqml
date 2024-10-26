import QtQuick
import QtQuick.Controls
import QtQuick.Controls.Basic
import "." as Ext

Popup {
  objectName: "clipboard_menu"
  x: (main.width - width) / 2
  y: 4

  Row {
    id: menuButtonRow
    spacing: 6

    Ext.MenuButton {
      objectName: "select_all"
      text: "\uf07d"
    }
    Ext.MenuButton {
      objectName: "cut"
      text: "\uf0c4"
    }
    Ext.MenuButton {
      objectName: "copy"
      text: "\uf0c5"
    }
    Ext.MenuButton {
      objectName: "paste"
      text: "\uf0ea"
    }
    Ext.MenuButton {
      objectName: "eval_exp"
      text: "\u03bb" // lambda
    }
  }
}
