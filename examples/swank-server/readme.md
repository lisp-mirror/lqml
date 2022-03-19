
Prepare
-------

Please copy the app template files first:
```
$ cd ..
$ ./copy.sh swank-server
```

See also [../../slime/src/readme-sources](../../slime/src/readme-sources.md) for
installing the Slime sources where this example can find them.



Info
----

Meant for mobile only. Provides both a **Swank server** and **Quicklisp**.

A simple REPL is integrated in order to start Swank with `:s`, and enable
Quicklisp with `:q`.

In the **iOS simulator** you need to run `(qrun* :s)` and `(qrun* :q)` instead,
otherwise the app will crash.

The most convenient way to connect from Slime is entering the IP address of the
mobile device after `M-x slime-connect`. You may need to detach your device
from USB for this to work.

**Quicklisp** note: it's always preferable to install Quicklisp and any library
from Slime on the desktop connected to the mobile device. Otherwise you won't
see the progress or any eventual problem during the process.



Quicklisp note
--------------

On **andoid**, Quicklisp is directly downloaded and installed.

On **iOS** the above method would crash ECL; this seems to be caused by the
limited stack size on iOS.

So, in order to use Quicklisp on iOS, you need to bring it yourself, which
means:

* install a fresh copy of Quicklisp on some desktop device, and copy over the
  whole directory `quicklisp` under `platforms/ios/assets/Library/` of this
  example



QML auto reload on mobile
-------------------------

If you compile for mobile, it will ask for the **Wifi IP** of your desktop
computer (currently hard coded into the app). If you just hit RET, auto
reloading will be disabled.

After installing and launching the app, just run this script from your example
directory:
```
./web-server.sh
```
It requires Python 3 and the cgi module, which are probably already installed
on your computer.

You may now edit any QML file on the desktop computer, and upon saving, all of
QML will be reloaded automatically. After reloading, the following file will be
loaded for eventual re-initialization on Lisp side:
```
lisp/qml-reload/on-reloaded.lisp
```
For **android**, in order to see the debug output of eventual QML errors, you
need to run `./log.sh` in your `build-android/` directory.

If you don't want auto reload enabled, comment out `auto-reload-qml` in
`lisp/main.lisp`.

Both desktop and mobile auto reload can also be run **simultaneously**, since
they share the QML source files. Any number of mobile devices may be connected,
if they are in the same WiFi and point to the same desktop IP.



Important notes for mobile
--------------------------

Please remember that installing a new version of your app on mobile will
**keep all app data** present on the device.

So, if you changed e.g. your Slime version under `platforms/.../assets/`, an
update will not replace those files, because they have been copied from the
assets on the very first start of the app.

A simple way to guarantee a clean install is simply uninstalling the app first,
both on the device and on the emulator (android) or simulator (iOS).

If your local web server doesn't seem to work, please check your firewall
settings.
