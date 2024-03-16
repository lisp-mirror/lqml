import QtQuick
import QtQuick.Controls
import QtQuick.Controls.Basic

Rectangle {
  id: help
  y: -rootItem.height
  color: "#e0e0f0"
  opacity: 0

  Button {
    width: 42
    height: width
    z: 1 // stay on top
    anchors.right: parent.right
    flat: true
    font.family: fontText.name
    font.pixelSize: 22
    text: "x"

    onClicked: help.enabled = false
  }

  Flickable {
    id: flick
    anchors.fill: parent
    contentWidth: html.paintedWidth + 2 * html.padding
    contentHeight: html.paintedHeight + 50
    clip: true

    Text {
      id: html
      width: help.width
      padding: 10
      wrapMode: Text.WordWrap
      font.family: fontText.name
      font.pixelSize: 18
      color: "#303030"
      textFormat: Text.RichText
      text: "
<h3>
<img src='../../img/radio.png' width=60 height=60>
<br>Radios
</h3>
<p>
If you use more than 1 radio, switch here to the radio you want to use.
</p>
<p>
To manually restart device discovery, press-and-hold on the radio icon.
</p>
%1
<h3>
<img src='../../img/group.png' width=60 height=60>
<br>Group
</h3>
<p>
Here you can see the list of all radios using your same channel name. Every radio represents a person. This view is populated automatically.
</p>
<p>
Choose 'Broadcast' (on top) to send a message to every person in the group.
</p>
<p>
You can set a name to every radio/person listed here, which defaults to 'Anonym': a press-and-hold on the name will enter edit mode.
</p>
<p>
In the main menu you can change your channel name (which defaults to 'LongFast'). Only radios which share the same channel name are able to exchange messages.
</p>
<p>
A tap on the location item on the right shows a map with all known positions of the persons. The map is cached automatically for offline usage, which means: once you visited a place on the map, it will remain available even without internet connection.
</p>
<p>
To set your location manually, see 'hand' button (top right). This will override any eventually received GPS location.
</p>
<h3>
<img src='../../img/message.png' width=60 height=60>
<br>Messages
</h3>
<p>
Since the message length is limited, the border of the editor will turn red if the message is too long for sending.
</p>
<p>
To copy a message to the clipboard, press-and-hold it.
</p>
<p>
To see the exact date of a message, tap on its hour.
</p>
<p>
To delete a message, swipe it to the right and tap on the delete button.
</p>
<p>
Tap on search (icon on the right) to enter/leave search mode. The search term (case insensitive) is highlighted in red.
</p>
<p>
Eventual unread messages from other persons are indicated by a red circle, and the number of unread messages in <b>Group</b>.
</p>
<p>
A double click on a message will switch to <b>Group</b>.
</p>
<p>&nbsp;</p>
<h3>Advanced topics</h3>
<h4>Simple signal strength test</h4>
<p>
For a trivial signal test you can use the special text message <b>:e</b> (for 'echo'), which will send back the text you sent, adding signal <b>SNR</b>/<b>RSSI</b>, position and distance. This is convenient to test signal strength from different places, and have it logged in your messages.
</p>
<p>
Please note that this requires the receiver to run this app in foreground mode.
</p>
<h4>Save / Restore app data</h4>
<p>
A local web-server is included on mobile for saving and restoring all of: message database, app settings, eventually cached map tiles (for offline usage). Just use special text message <b>:w</b> (for 'web-server') and <b>:ws</b> (for 'stop web-server') after you're done.
</p>
<p>
After starting the server, enter the shown URL in your desktop browser, and follow the instructions.
</p>
<p>
Using this method you can easily transfer all data from one mobile device to any other device.
</p>
<p>
The desktop data paths are:
<ul>
<li><b>Linux</b>: <br><code>/home/&lt;user&gt;/.local/share/cl-meshtastic/</code>
<li><b>macOS</b>: <br><code>/Users/&lt;user&gt;/Library/Application Support/cl-meshtastic/</code>
<li><b>Windows</b>: <br><code>C:\\Users\\&lt;user&gt;\\AppData\\Local\\cl-meshtastic\\</code>
</ul>
<p>
Eventual backups are saved in above path under <code>backups/</code>. On the desktop see 'Make backup' in main menu.
</p>
<p>
To autmatically restore data from a backup on the desktop, put the backup files directly in above path (that is, under <code>.../cl-meshtastic/</code>) and restart the app. The data will be restored and the (obsolete) backup files will be deleted.
</p>".arg((Qt.platform.os === "android")
          ? "<p>On some devices it may be necessary to first unpair your radio, then press-and-hold on the radio icon (to restart device discovery).</p><p><i>N.B: If you previously used a radio with the official app, you'll need to set the radio to 'None (disabled)' in the official app first, otherwise it will not show up in this app.</i></p>"
          : "")

    }
  }

  states: [
    State { when: help.enabled;  PropertyChanges { target: help; opacity: 1; y: 0; }},
    State { when: !help.enabled; PropertyChanges { target: help; opacity: 0; y: -rootItem.height; }}
  ]

  transitions: [
    Transition { NumberAnimation { properties: "opacity,y"; duration: 300; easing.type: Easing.InOutQuad }}
  ]
}
