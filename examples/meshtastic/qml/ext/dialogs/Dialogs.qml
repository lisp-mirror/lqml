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
    loader.active = false // force reload
    if (rootItem.mobile) {
      loader.source = "MessageMobile.qml"
    } else {
      loader.source = "Message.qml"
    }
    loader.active = true
    loader.item.text = text
    rootItem.showKeyboard(false)
    loader.item.open()
  }

  function confirm(title, text, callback, from, to, value) {
    loader.active = false // force reload
    if (rootItem.mobile) {
      loader.source = "ConfirmMobile.qml"
    } else {
      loader.source = "Confirm.qml"
    }
    loader.active = true
    loader.item.title = title
    loader.item.text = text
    loader.item.callback = callback
    loader.item.from = from
    loader.item.to = to
    loader.item.value = value
    rootItem.showKeyboard(false)
    loader.item.open()
  }
}
