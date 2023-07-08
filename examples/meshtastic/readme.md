
Info
----

Please note: this is **WIP!**

Currently it can be used to send direct messages between any number of radios.
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

The message db uses **sqlite**, but in a lispy manner, storing basically just a
plist for every message.

The reason I chose Qt qsqlite over cl-sqlite is mobile: Qt comes with its own
version, which is pulled in automatically, so one doesn't need to care about
the OS limitations or indirect requirements. Additionally, cffi (as a
dependency of cl-sqlite) currently needs a small hack to even work on mobile.



Tested
------

Tested on Linux, macOS, android, iOS. The macOS version shows an ECL exception
during BLE ini, but works nevertheless.

It should also work on Windows >= 10, but this is not tested yet.

Since this is WIP, it may currently not work on all platforms (e.g. mobile).



How to use cl-meshtastic
------------------------

Your radio needs to be turned on and bluetooth needs to be enabled before you
start the app.

On android coarse location permission is required for BLE to work.

Pairing of your LoRa radios is generally not needed beforehand, the app will
ask for pairing/PIN during BLE ini. If your device doesn't have a display, use
`123456` as your PIN.

It may occur that the devices are sometimes not found; in those cases

* try to turn bluetooth off and on again, and/or:
* try to reboot your radios, and/or:
* try to unpair your radios from all computers/devices

A generic bluetooth app like **nRF Connect** may help in order to see if the
devices themselves work and are able to connect.

See also [readme-usage](readme-usage.md).



TODO / ideas
------------

(1) Since this uses Lisp, there's the possibility to integrate a programmable
interface, where users can define their own functions and extend the UI.

Think of simple scripts, which send protobufs to the radio and process the
received data, while having access to all the variables and functions of
the app itself.

Those extensions could also be shared among users.

(2) Regarding encryption: since I don't like QR codes for sharing a channel,
there's the possibility to just share a made up phrase which can easily be
remembered, and use the letters of the phrase as encryption key, so people can
share their channel in a simple way.



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

