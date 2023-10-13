import QtQuick 2.15
import QtQuick.Controls 2.15
import "." as Ext

Rectangle {
  id: emojis
  objectName: "emojis"
  height: 7 * itemSize + 2 * column.spacing + 8
  radius: 12
  color: "white"
  border.width: 1
  border.color: "gray"

  property int itemSize: 42

  Column {
    id: column
    anchors.fill: parent
    spacing: 5

    Ext.EmojiView {
      objectName: "recent_emojis"
      height: itemSize + 1
      ScrollBar.vertical.policy: ScrollBar.AlwaysOff

      model: ["🙂","🤣","👍"]
    }

    Ext.EmojiView {
      height: itemSize * 3 + 1

      model: ["😃","😄","😁","😆","😅","😂","🤣","🥲","🥹","😊","😇","🙂","🙃","😉","😌","😍","🥰","😘","😗","😙","😚","😋","😛","😝","😜","🤪","🤨","🧐","🤓","😎","🥸","🤩","🥳","😏","😒","😞","😔","😟","😕","🙁","😣","😖","😫","😩","🥺","😢","😭","😮‍💨","😤","😠","😡","🤬","🤯","😳","🥵","🥶","😱","😨","😰","😥","😓","🫣","🤗","🫡","🤔","🫢","🤭","🤫","🤥","😶","😶‍🌫️","😐","😑","😬","🫨","🫠","🙄","😯","😦","😧","😮","😲","🥱","😴","🤤","😪","😵","😵‍💫","🫥","🤐","🥴","🤢","🤮","🤧","😷","🤒","🤕","🤑","🤠","😈","👿","👹","👺","🤡","💩","👻","💀","👽","👾","🤖","🎃","😺","😸","😹","😻","😼","😽","🙀","😿","😾"]
    }

    Ext.EmojiView {
      height: itemSize * 3 + 1

      model: ["👋","🤚","🖐","✋","🖖","👌","🤌","🤏","🤞","🫰","🤟","🤘","🤙","🫵","🫱","🫲","🫸","🫷","🫳","🫴","👈","👉","👆","🖕","👇","👍","👎","✊","👊","🤛","🤜","👏","🫶","🙌","👐","🤲","🤝","🙏","💅","🤳","💪","🦾","🦵","🦿","🦶","👣","👂","🦻","👃","🫀","🫁","🧠","🦷","🦴","👀","👁","👅","👄","🫦","💋","🩸"]
    }
  }
}
