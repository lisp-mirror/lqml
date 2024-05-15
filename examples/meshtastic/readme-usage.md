Help
----

Please see the 'Help' page included in the app (see main menu).


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


Hacker tips
-----------

If you are a Lisp hacker, you may enjoy the integrated Swank server (on
mobile). Just type special text message `:s`. A message with the IP to connect
to will be shown once the server is running. Beware though that Swank on mobile
isn't very stable, but it's perfect for simple debugging purposes, or to
get/set variables on the fly (but it might crash regularily if you try to eval
some buffer, or even during auto-completion).

For full Swank/Slime power you'll need the desktop version anyway (this is how
this app is being developed).

