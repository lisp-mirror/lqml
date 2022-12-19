import QtQuick 2.15
import QtQuick.Controls 2.15

TextField {
  font.pixelSize: 18 + main.small ? 0 : 2
  palette {
    highlight: "#007aff"
    highlightedText: "white"
  }
}
