import QtQuick 2.15
import QtQuick.Controls 2.15

ComboBox {
  id: control
  font.pixelSize: 16
  font.family: fontText.name

  delegate: ItemDelegate {
    width: control.width
    height: control.height
    contentItem: Text {
      text: modelData
      font: control.font
      horizontalAlignment: Text.AlignHCenter
      verticalAlignment: Text.AlignVCenter
    }
    highlighted: control.highlightedIndex === index
  }

  contentItem: Text {
    text: control.displayText
    font: control.font
    horizontalAlignment: Text.AlignHCenter
    verticalAlignment: Text.AlignVCenter
  }

  background: Rectangle {
    radius: 5
    color: "#f0f0f0"
  }

  popup: Popup {
    objectName: "popup"
    y: control.height
    width: control.width + 24
    implicitHeight: contentItem.implicitHeight + 14

    contentItem: ListView {
      clip: true
      implicitHeight: contentHeight + 10
      model: control.popup.visible ? control.delegateModel : null
      currentIndex: control.highlightedIndex
    }

    background: Rectangle {
      color: "#e0e0e0"
      border.width: 1
      border.color: "gray"
      radius: 10
    }
  }
}
