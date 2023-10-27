Info
----

This describes how to use this specific meshtastic app, because it differs from
the official apps.

The main focus here is simple, basic usage, like in an emergency situation,
where we don't need all the special features, just basic communication.

You can send both direct and broadcast messages to any device which is within
the range of your mesh network.

Starting this app will automatically do a basic setup of your radio, so no
additional setup is needed.


Initial setup
-------------

Every person which you want to communicate with will need to start the app once
for the initial (automatic) setup. The radio device will then reboot, and is
ready for communication with all the persons who also use this app (and which
radios can be reached through the mesh network).


3 main views
------------

Following there is a description of the 3 main views:

* Group
* Messages (initial view)
* Radios


Messages
--------

The initial view shows the messages between you and a chosen person. You choose
the desired person in the **Group** view (tap on group icon).

To copy a message to the clipboard, press-and-hold it.

To see the exact date of a message, tap on its hour.

To delete a message, swipe it to the right and tap on the delete button.

The search function (icon on the right) should be intuitive. The search term
(case insensitive) is highlighted in red. Tap again on the search icon to leave
search mode.

A double click on a message will change to Group.


Group
-----

Tapping on group (in **Messages**), you'll see the list of all persons. Every
person is associated to a specific radio. This view is populated automatically.

Choose 'Broadcast' (on top) to send a message to every person in the group.

You can set a name to every person associated to a radio, which defaults to
'Anonym': a press-and-hold on the name will enter edit mode.

A tap on the location item on the right shows a map with all known positions of
the persons. The map tiles are cached automatically for offline usage, which
means: once you visited a place on the map, it will remain available even
without internet connection.


Radios
------

Tapping on radio (in **Messages**), you'll see the list of all radios
visible to your phone's bluetooth. You generally just choose 1 device, which
will be remembered.

If you use more than 1 radio, just switch here to the radio you want to use.

Changing a radio will take several seconds, because the initial configuration
needs to be repeated.

Initially, the list will contain gray items, so you can choose another radio
if you experience that the connection didn't work, in case the selected radio
is currently not available.

In the rare case your radio is not found, you may restart device discovery by a
press-and-hold on the radio icon. If it still isn't found, you may try to
unpair the radio from any device you paired it previously.


Unread messages
---------------

When you are in the **Messages** view, a red circle on the group icon will
inform you of new, unread messages from another user.

Switching to **Group**, a red circle with the number of unread messages is
shown on the right of every person.


GPS position
------------

On mobile, the location of the phone is sent once an hour to the radio.

It's also possible to set a position manually, which will override any GPS
position, see 'hand' button when the map is shown.


Save / Restore data
-------------------

A local web-server is included on mobile for saving and restoring all of:
message DB, app settings, eventually cached map tiles (for offline usage).
Just use special text message `:w` (for 'web-server') and `:ws` (for 'stop
web-server') after you're done.

After starting the server, just enter the shown URL in your desktop browser,
and follow the instructions.

Using this method you can easily transfer all data from one mobile device to
any other device.

The desktop data paths are:

* Linux: `/home/<user>/.local/share/cl-meshtastic/`
* macOS: `/Users/<user>/Library/Application Support/cl-meshtastic/`
* Windows: `C:\Users\<user>\AppData\Local\cl-meshtastic\`

Eventual backups are saved in above path under `backups/`. On the desktop
see 'Make backup' in menu (tap on logo on top left).

To autmatically restore data from a backup on the desktop, put the backup files
directly in above path (that is, under `.../cl-meshtastic/` and restart the
app. The data will be restored and the (obsolete) backup files will be deleted.


Tips
----

If (for some reason) you want to redo the bluetooth discovery of your radio(s),
just press-and-hold on the **Radios** icon.

If (for some reason) you want to receive again the mesh node configuration from
your radio, just press-and-hold on the **Group** icon.

Both of above is meant to avoid app restart.

For a trivial signal test you can use the special text message `:e` (for
'echo'), which will send back the text you sent, adding signal SNR/RSSI,
position and distance. This is convenient to test signal strength from
different places, and have it logged in your messages.


Hacker tips
-----------

If it occurs that a radio device goes into an undefined state and doesn't seem
to work anymore, you can try a factory reset (see CLI) and flash the latest
firmware.

RAK devices can also completely erase the flash memory from an arduino IDE,
see RAK on github and file `reset-flash.ino` (re-flash firmware afterwards).

If you are a Lisp hacker, you may enjoy the integrated Swank server (on
mobile). Just type special text message `:s`. A message with the IP to connect
to will be shown once the server is running. Beware though that Swank on mobile
isn't very stable, but it's perfect for simple debugging purposes, or to
get/set variables on the fly (but it might crash regularily if you try to eval
some buffer, or even during auto-completion).

For full Swank/Slime power you'll need the desktop version anyway (this is how
this app is being developed).

