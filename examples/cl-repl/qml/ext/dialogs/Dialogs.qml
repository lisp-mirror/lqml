import QtQuick 2.15

Item {
  id: dialogs
  objectName: "dialogs"
  anchors.fill: parent

  Loader {
    id: loader
    anchors.centerIn: parent
  }

  function message(text) {
    if ((Qt.platform.os === "android") ||
        (Qt.platform.os === "ios")) {
      loader.source = "MessageMobile.qml"
    } else {
      loader.source = "Message.qml"
    }
    loader.item.text = text
    loader.item.open()
  }

  function confirm(title, text, callback) {
    if ((Qt.platform.os === "android") ||
        (Qt.platform.os === "ios")) {
      loader.source = "ConfirmMobile.qml"
    } else {
      loader.source = "Confirm.qml"
    }
    loader.item.title = title
    loader.item.text = text
    loader.item.callback = callback
    loader.item.open()
  }
}
