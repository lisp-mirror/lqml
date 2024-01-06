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

  function confirm(text, callback) {
    loader.active = false // force reload
    if (rootItem.mobile) {
      loader.source = "ConfirmMobile.qml"
    } else {
      loader.source = "Confirm.qml"
    }
    loader.active = true
    loader.item.text = text
    loader.item.callback = callback
    rootItem.showKeyboard(false)
    loader.item.open()
  }

  function input(title, label, callback, text, maxLength, from, to, value) {
    loader.active = false // force reload
    if (rootItem.mobile) {
      loader.source = "InputMobile.qml"
    } else {
      loader.source = "Input.qml"
    }
    loader.active = true
    loader.item.title = title
    loader.item.label = label
    loader.item.callback = callback
    loader.item.text = text
    loader.item.maxLength = maxLength
    loader.item.from = from
    loader.item.to = to
    loader.item.value = value
    var keyboard = (text !== "")
    rootItem.showKeyboard(keyboard)
    loader.item.open()
    if (keyboard) loader.item.focus()
  }
}
