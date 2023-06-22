
Info
----

Please note: this is **WIP!** It's only a 'proof-of-concept' version.

Eventually it will (hopefully) catch up with the official app versions.
 


Technical notes
---------------

This app uses both **BLE** (bluetooth low energy) and the **protobufs**
serialization library version 3.

For BLE you need to build the plugin in `cpp/` first (written in Qt5).

The cl-protobufs library is included here because the official version doesn't
work without the C++ plugin installed (which we don't need here). So I made
some small adaptions and included all generated proto Lisp files in order to be
independent.

Unfortunately cl-protobufs loads very slowly on mobile (and conses hugely
during startup). On an older phone and a cold startup this may take more than
20 seconds. On newer phones and warm startup it should 'only' take around 10
seconds (which seems acceptable).

For the above reason, an animation is shown while loading the app, together
with a counter. For this to work, the app is loaded in the background (that is,
in a separate thread). You'll need to rebuild the lqml library for this to
work.

You will see a json output of all data sent/received. It simply uses the
`print-json` convenience function from cl-protobufs.



Tested
------

Tested on Linux, macOS, android, iOS. The macOS version shows an ECL exception
during BLE ini, but works nevertheless.



How to use cl-meshtastic
------------------------

You currently need exactly 2 meshtastic radio devices, both should be running
before you start the app. Both bluetooth and location needs to be enabled
(coarse location permission is required on android for BLE to work).

Pairing might sometimes require some playing around. If it asks for a PIN and
your device doesn't have a display (like the RAK starter kit), just use
`123456`.

Pairing of your LoRa radios is generally not needed beforehand, except on
android, where you need to start the app only after successful pairing.

If your android phone says "no BLE devices found" (see logcat output), you
might need to unpair the devices and pair them again.

On Linux you might sometimes also need to unpair and pair again, if there are
errors when trying to connect.



Run
---
```
lqml run.lisp
```
Optionally pass `-slime` to start a Swank server, and connect from Emacs with
`M-x slime-connect`.

During development you can pass `-auto`, which will reload all QML files after
you made a change to any of them and saved it. For re-initialization after
reloading, file `lisp/qml-reload/on-reloaded` will be loaded.

Closing the window quits the app. If you try to kill it with `ctrl-c`, you need
an additional `ctrl-d` to exit from ECL. To quit from Slime, do `(qq)` which is
short for `(qquit)`.

