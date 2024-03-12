import QtQuick
import QtQuick.Controls
import QtQuick.Controls.Basic

ScrollView {
  width: parent.width
  ScrollBar.vertical.policy: ScrollBar.AlwaysOn
  ScrollBar.horizontal.policy: ScrollBar.AlwaysOff

  property alias model: grid.model

  GridView {
    id: grid
    anchors.fill: parent
    cellWidth: emojis.itemSize
    cellHeight: emojis.itemSize
    leftMargin: 2
    topMargin: 2
    clip: true
    highlightFollowsCurrentItem: false
    focus: true

    delegate: Text {
      width: emojis.itemSize
      height: emojis.itemSize
      horizontalAlignment: Text.AlignHCenter
      verticalAlignment: Text.AlignVCenter
      font.pixelSize: emojis.itemSize - 4
      text: modelData
    }

    MouseArea {
      anchors.fill: parent
      cursorShape: Qt.PointingHandCursor

      onClicked: Lisp.call("app:emoji-clicked",
                           grid.itemAtIndex(grid.indexAt(mouse.x, mouse.y + grid.contentY)).text)
    }
  }
}
