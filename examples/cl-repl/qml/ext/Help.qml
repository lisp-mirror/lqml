import QtQuick 2.15
import "." as Ext

Rectangle {
  color: "lightyellow"
  visible: false

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
      contentHeight: help.paintedHeight + 100

      Text {
        id: help
        width: flick.width
        padding: 10
        wrapMode: Text.WordWrap
        font.pixelSize: 18
        textFormat: Text.RichText
        text:
"
<h2>Help</h2>
<table cellpadding=5>
  <tr>
    <td align=right><b>:?</b></td><td>find regular expression, e.g.<b> :?&nbsp;prin[c1]</b>; hit RET for next match</td>
  </tr>
  <tr>
    <td align=right><b>*</b></td><td>copy eval value to clipboard</td>
  </tr>
  <tr>
    <td align=right><b>:c</b></td><td>clear all output</td>
  </tr>
  <tr>
    <td align=right><b>:k</b></td><td>kill eval thread (long running task)</td>
  </tr>
  <tr>
    <td align=right><b>:s</b></td><td>start Swank server</td>
  </tr>
  <tr>
    <td align=right><b>:q</b></td><td>load Quicklisp</td>
  </tr>
  <tr>
    <td align=right><b>:w</b></td><td>start local web-server for file upload/download, see http://192.168.1.x:1701/</td>
  </tr>
  <tr>
    <td align=right><b>:ws</b></td><td>stop local web-server</td>
  </tr>
  <br>
  <tr>
    <td align=right><b>double SPC</b></td><td>auto completion, e.g.<b> m-v-b</b></td>
  </tr>
  <tr>
    <td align=right><b>tap and hold</b></td><td>in editor to select/copy/paste/eval s-expression, e.g. on <b>defun</b></td>
  </tr>
  <tr>
    <td align=right><b>tap and hold</b></td><td>cursor buttons to move to beginning/end of line/file</td>
  </tr>
  <br>
  <tr>
    <td align=right><b>[Up]</b></td><td>move back in eval line history</td>
  </tr>
  <tr>
    <td align=right><b>[Down]</b></td><td>move forward in eval line history</td>
  </tr>
  <tr>
    <td align=right><b>[Tab]</b></td><td>switch focus between editor / eval line</td>
  </tr>
  <tr>
    <td align=right><b>[%1+E]</b></td><td><b>E</b>xpression: select s-exp</td>
  </tr>
  <tr>
    <td align=right><b>[%1+L]</b></td><td><b>L</b>ambda: eval selected s-exp</td>
  </tr>
</table>
".arg((Qt.platform.os === "ios")
      ? "Alt" : ((Qt.platform.os === "osx")
                 ? "Cmd" : "Ctrl"))
      }
    }
  }
}
