import QtQuick 2.15
import "." as Ext

Rectangle {
  color: "lightyellow"

  Column {
    anchors.fill: parent

    Ext.MenuBack {
      label: "Help"
    }

    Ext.Flickable {
      id: flick
      width: parent.width
      height: parent.height
      contentWidth: help.paintedWidth
      contentHeight: help.paintedHeight + 75

      Text {
        id: help
        width: flick.width
        height: flick.height
        padding: 20
        wrapMode: Text.WordWrap
        font.pixelSize: 20
        text:
        "<h2>Help</h2>
           <ul>
           <p><b>:?</b> find regular expression, e.g. <b>:?&nbsp;prin[c1]</b></p>
           <p><b>*</b> copy result to clipboard</p>
           <p><b>:s</b> start swank server</p>
           <p><b>:c</b> clear all output</p>
           <p><b>:k</b> kill eval thread (long running task)</p>
           <p><b>double [Space]</b> auto completion, e.g. <b>m-v-b</b></p>
           <p><b>tap and hold</b> in the editor to select/copy/paste/eval s-expression, e.g. on <b>defun</b></p>
           <p><b>tap and hold</b> cursor buttons to move to beginning/end of line/file</p>
        </ul>"
      }
    }
  }
}
