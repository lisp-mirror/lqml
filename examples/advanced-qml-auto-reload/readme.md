
Prepare
-------

Please copy the app template files first:
```
$ cd ..
$ ./copy.sh advanced-qml-auto-reload
```

See also [../../slime/src/readme-sources](../../slime/src/readme-sources.md) for
installing the Slime sources where this example can find them.



Info
----

This is simply an extended version of example **swank-sevrer** of the parent
directory.

It adds a QML `SwipeView` with 3 pages, to demonstrate how to reload single
QML pages, without reloading the whole UI. This is important for nested UI
pages, in order to not lose your current view in the UI.

Without the aboce feature, you would always land on the main view after
reloading QML.



QML single file auto reload on mobile
-------------------------------------

If you compile for mobile, it will ask for the **Wifi IP** of your desktop
computer (currently hard coded into the app). If you just hit RET, auto
reloading will be disabled.

N.B: **Before** installing and launching the app, just run this script from
your example directory:
```
./web-server.sh
```
It requires Python 3 and the cgi module, which are probably already installed
on your computer.

You may now edit any QML file on the desktop computer, and upon saving, only
the saved file will be reloaded.

Only if you edit and save `qml/main.qml` the following file will be loaded for
eventual re-initialization on Lisp side:
```
lisp/qml-reload/on-reloaded.lisp
```
For **android**, in order to see the debug output of eventual QML errors, you
need to run `./log.sh` in your `build-android/` directory.

If you don't want auto reload enabled (would block the app if the web server
isn't reachable), you need to rebuild the app and skip entering an IP during
the build process, see above.

Both desktop and mobile auto reload can also be run **simultaneously**, since
they share the QML source files. Any number of mobile devices may be connected,
if they are in the same WiFi and point to the same desktop IP.


Important notes for mobile
--------------------------

Please remember that installing a new version of your app on mobile will
**keep all app data** present on the device.

So, if you changed e.g. `lisp/curl.lisp`, a simple update will not replace
that file, because it has been copied from the assets directory, and is only
replaced if you increment `+app-version+` in `lisp/swank-quicklisp.lisp`.

A simple way to guarantee a clean install is simply uninstalling the app first,
both on the device and on the emulator (android) or simulator (iOS).

If your local web server doesn't seem to work, please check your firewall
settings.
