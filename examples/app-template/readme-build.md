
Prepare
-------

If you use Qt versions prior to 5.15, you need to adapt the QML module version
to your minor Qt version in all files under `qml/`.

Example: Qt5.**12** => import QtQuick 2.**12**.

The version number can be omitted in Qt6.


Note
----

Every Lisp file under `lisp/` and every qml/image/font/whatever file under
`qml/` is added **automatically** to your Qt project file (both for re-compile
and adding to resources included in the executable).

So, you only need to **manually** care about the usual ASDF project files in
`app.asd`.


Run desktop
-----------
```
$ lqml run.lisp
```


Build desktop app
-----------------
```
$ cd build

$ qmake ..
$ make
```


Build android APK
-----------------
```
$ cd build-android

$ qmake-android ..
$ make apk

$ ./install-run.sh
```
Log note: for showing only your own messages, see `log.sh`.



Build iOS app
-------------

**Important notes**: the Qt Xcode integration is not perfect, which means: when
you include asset files (like in example `swank-server`), they may not be
copied to the build directory, the first time you build the app.

So, it's a good idea to run `qmake-ios` again if there are any startup problems
of the app (like asset files not found when launching).

The first build after a `qmake-ios` will almost always fail (missing build
files): don't worry, just run 'Build' (from Xcode) again, and the missing files
will be created.

You also need to check the console from where you launched `./xcode.sh` for
eventual errors compiling the Lisp code.
```
$ cd build-ios

$ qmake-ios ..

$ ./xcode.sh
```
The script above first cross-compiles the Lisp code, then opens **Xcode**.

Please note (important):

* before building the app, go to 'Build Settings' / 'Build Options' and set
  **Enable Bitcode** to **No**

* if it complains about missing source files when first hitting the 'Run'
  button, just hit the 'Run' button again (and they will be generated)

* using latest Xcode, it may complain about the 'Legacy Build System'; just go
  to 'File' / 'Project Settings' and select 'New Build System'

* you only need to run `qmake-ios` again after you added/removed files to the
  project; after every `qmake-ios`, the above steps need to be repeated

If you cross-compiled ECL for the simulator, it should work there too, but this
is currently only tested on **Intel**.

Simulator note: to show the virtual keyboard, use `cmd-k`.



Notes
-----

You will note that sometimes a change of a single Lisp file won't recompile
that file on the next `make`; in those cases, just do something like
`touch ../app.asd` to force recompilation of everything.

For conditions during Qt event processing, a fallback restart is added at
startup (needed in e.g. Slime).

If you don't want this, define the following in `app.pro`:
```
DEFINES += NO_QT_RESTART
```


Translations (i18n)
-------------------

Please see [i18n/readme](i18n/readme.md).
