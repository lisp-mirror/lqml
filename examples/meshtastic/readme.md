
Info
----

An APK (android) can be found here:
[meshtastic.apk](https://www.dropbox.com/scl/fi/bch0bay4ztk1a1j77nwqf/meshtastic.apk?rlkey=lkjbrz41760u54qaexkd1xz68&dl=0)

This simple app can either send direct messages between the radios, or
broadcast messages (received by all radios).

It's basically meant to be used in an emergency situation, where internet is
not available, in order to communicate with short text messages. This kind of
mesh network is limited to about 70 nodes/radios/users to remain reliable.


Technical notes
---------------

You'll probably need to rebuild the lqml library, this app requires the latest
version.

The app uses both **BLE** (bluetooth low energy) and the **protobufs**
serialization library version 3.

For BLE you need to build the plugin in `cpp/` first (written in Qt5/Qt6).

The cl-protobufs library is included here because the official version doesn't
work without the C++ plugin installed (which we don't need here). So I made
some small adaptions and included all generated proto Lisp files in order to be
independent.

Unfortunately the generated and C compiled meshtastic proto files load very
slowly on mobile. To improve load time, meshtastic proto files are simply
loaded as `*.lisp` source files, which doesn't seem to impact performance at
runtime (in this use case).

An hourglass animation is shown while loading the app. For this to work, the
app is loaded in the background (that is, in a separate thread).

You will see a json output of all data sent/received. It simply uses the
`print-json` convenience function from cl-protobufs.

The message db uses **sqlite**, but in a lispy manner, storing basically just a
plist for every message.

The reason I chose Qt qsqlite over cl-sqlite is mobile: Qt comes with its own
version, which is pulled in automatically, so one doesn't need to care about
OS limitations or indirect requirements.



Tested
------

Tested on Linux, macOS, Windows 10+, android, iOS, SailfishOS.

The macOS version must be built with Qt6 (Qt5 has a buggy BLE implementation),
and compiled first (see `build/`). So, you'll need to rebuild lqml using Qt6,
and replace `qml/` with `../meshtastic-qt6/qml/`.

The iOS version also runs on older devices, like the 'iPod touch', as long as
they are 64 bit and run at least iOS 12.



How to use cl-meshtastic
------------------------

## Bluetooth

The most universal connection type is BLE (Bluetooth Low Energy), which should
work everywhere.

Your radio needs to be turned on and bluetooth needs to be enabled before you
start the app.

On android coarse location permission is required for BLE to work.

Except for Windows, pairing of your LoRa radios is generally not needed
beforehand, the app will ask for pairing/PIN during BLE ini. If your device
doesn't have a display, use `123456` as your PIN.

On Windows it didn't work for me if not paired previously.

It may occur that your radio device is sometimes not found; some suggestions:

* try to disable the radio in any other app that used it previously
* try to unpair your radio from current device
* reboot your radio (turn off/on)
* press-and-hold on the radio icon to restart device discovery

A generic bluetooth app like **nRF Connect** may help in order to see if the
devices themselves work and are able to connect.


## USB

If you have your radio connected to your PC via USB, you can also choose 'USB'
as connection type from the main menu (see 'Radios' icon). This only works on
Linux and macOS (Windows not tested). RAK devices won't need any additional
driver, just permission to e.g. `/dev/ttyACM0` (Linux). Other radios (ESP32
based) require addtional serial drivers to be installed, see official
Meshtastic website.

## WiFi

If you have a radio with intgrated WiFi (like LILYGO T-BEAM, HELTEC V3), you
can setup your WiFi connection using the Python CLI:
```
meshtastic \
--set network.wifi_enabled true \
--set network.wifi_ssid "<name>" \
--set network.wifi_psk "<password>"
```
You should now be able to see the IP of your WiFi connected radio on it's
small display (use the user button to switch to the WiFi page). The app will
ask for the IP if not entered yet, or if it has changed.

See also [readme-usage](readme-usage.md).



Emojis (Linux/SailfishOS)
-------------------------

See [emojis-howto](platforms/linux/emojis-howto.md).



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


Hardware example
----------------

See [radio](hardware/radio.htm) for a simple radio example and some basics you
should know about antennas.
