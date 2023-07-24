Info
----

This describes how to use this specific meshtastic app, because it differs from
the official apps.

The main focus here is simple, basic usage, like in an emergency situation,
where we don't need all the special features, just basic communication.

Currently you can only send direct messages to any device which is within the
range of your mesh network.

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
the desired person in the **Group** view (swipe to the left).

To copy a message to the clipboard, simply press-and-hold it.

The search function (icon on the right) should be intuitive. The search term
(case insensitive) is highlighted in red. Tap again on the search icon to leave
search mode.


Group
-----

Swiping to the left (from **Messages**), you see the list of all persons. Every
person is associated to a specific radio. This view is populated automatically.

You can set a name to every person associated to a radio, which defaults to
'Anonym': a press-and-hold on the name will enter edit mode.


Radios
------

Swiping to the right (from **Messages**), you see the list of all radios
visible to your phone's bluetooth. You generally just choose 1 device, which
will be remembered.

If you use more than 1 radio, just switch here to the radio you want to use.

Changing a radio will take several seconds, because the initial configuration
needs to be repeated.

Initially, the list will contain gray items, so you can choose another radio
if you experience that the connection didn't work, in case the selected radio
is currently not available.


Unread messages
---------------

When you are in the **Messages** view, a red circle on the group icon will
inform you of new, unread messages from another user.

Switching to **Group**, a red circle with the number of unread messages is
shown on the right of every person.


GPS position
------------

On mobile, and if the radio doesn't have a GPS module, the location of the
phone is sent once (at startup) to the radio.


Tips
----

If (for some reason) you want to redo the bluetooth discovery of your radio(s),
just press-and-hold on the **Radios** icon.

If (for some reason) you want to receive again the mesh node configuration from
your radio, just press-and-hold on the **Group** icon.

Both of above is meant to avoid app restart.


Hacker tips
-----------

If it occurs that a RAK device goes into an undefined state and doesn't seem
to work anymore, you can try to reset the flash memory from an arduino IDE,
see the RAK github and file `reset-flash.ino`. I successfully recovered a
RAK device with corrupted memory using this method.
