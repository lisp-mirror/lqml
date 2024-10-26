import QtQuick

Item {
  id: dialogs
  objectName: "dialogs"
  anchors.fill: parent

  Loader {
    id: loader
    anchors.centerIn: parent
  }

  function message(text) {
    if (Lisp.call("qml:mobile-p")) {
      loader.source = "MessageMobile.qml"
    } else {
      loader.source = "Message.qml"
    }
    loader.item.text = text
    main.showKeyboard(false)
    loader.item.open()
  }

  function confirm(title, text, callback) {
    if (Lisp.call("qml:mobile-p")) {
      loader.source = "ConfirmMobile.qml"
    } else {
      loader.source = "Confirm.qml"
    }
    loader.item.title = title
    loader.item.text = text
    loader.item.callback = callback
    main.showKeyboard(false)
    loader.item.open()
  }
}
