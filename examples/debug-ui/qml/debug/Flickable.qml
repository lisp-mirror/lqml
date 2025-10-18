import QtQuick 2.15
import QtQuick.Controls 2.15
import "." as Ext

Flickable {
  clip: true

  ScrollBar.vertical: Ext.ScrollBar {}

  function ensureVisible(r) {
    if (main.skipEnsureVisible)
      return;
    if (contentX >= r.x)
      contentX = r.x;
    else if (contentX + width <= r.x + r.width)
      contentX = r.x + r.width - width;
    if (contentY >= r.y)
      contentY = r.y;
    else if (contentY + height <= r.y + r.height)
      contentY = r.y + r.height - height;
  }
}
