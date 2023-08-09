
Info
----

Please note: this is **WIP!**

Currently it can be used to send direct messages between any number of radios.

It's basically meant to be used in an emergency situation, where internet is
not available, in order to communicate with simple text messages. This kind of
mesh network is limited to about 70 nodes/radios/users to remain reliable.



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
OS limitations or indirect requirements. Additionally, cffi (as a dependency
of cl-sqlite) currently needs a small hack to even work on mobile.



Tested
------

Tested on Linux, macOS, android, iOS.

The macOS version must be compiled first and started from Finder (not the
console), otherwise BLE permissions will not work (if run from console, the app
will show a BLE exception and consume 100% CPU).

Windows 10+ will follow soon.



How to use cl-meshtastic
------------------------

Your radio needs to be turned on and bluetooth needs to be enabled before you
start the app.

On android coarse location permission is required for BLE to work.

Pairing of your LoRa radios is generally not needed beforehand, the app will
ask for pairing/PIN during BLE ini. If your device doesn't have a display, use
`123456` as your PIN.

It may occur that your radio device is sometimes not found; first

* try to press-and-hold on the radio icon (will restart device discovery)

Only if this doesn't work, you may

* try to turn bluetooth off and on again, and/or:
* try to reboot your radios, and/or:
* try to unpair your radios from all computers/devices

A generic bluetooth app like **nRF Connect** may help in order to see if the
devices themselves work and are able to connect.

See also [readme-usage](readme-usage.md).


Build
-----

Please apply [hacks](hacks/) before trying to build.

On **iOS** see also note in [upload-download.lisp](lisp/upload-download.lisp)
regarding function `read-sequence`, which needs to be replaced in the Quicklisp
**zip** library.


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

