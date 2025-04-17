
Prepare
-------

First you need to patch ASDF from ECL android. So, switch to where you have
ecl-android installed, and edit file: 'contrib/asdf/asdf.lisp'.

Find the following function, and replace it with this code:
```
(defun system-source-directory (system-designator)
  ;; hack
  (let ((dir (pathname-directory-pathname (system-source-file system-designator))))
    (if dir
        (translate-logical-pathname dir)
        (when (probe-file "/sdcard/") ; android (needs runtime check)
          *default-pathname-defaults*))))
```
You need to rebuild ECL android after this; in order to make a clean rebuild,
remove the following directories:
```
  rm -fr ecl-android
  rm -fr ecl-android-host
  rm -fr build
```
Then run the 2 build scripts again.

(Notes: this is needed because on android some paths are returned as logical
pathnames, which need translation for the way they are used in CLOG. And in
case the ASDF source files are not present, like here, the above path should
not return `nil`, which would give a runtime error, so we simply return the app
path instead.)

--

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

So, it's a good idea to run `qmake-ios` twice before building the app.

The first build after a `qmake-ios` will almost always fail (missing files):
just run 'Build' (from Xcode) again, and the missing files will be created.

You also need to check the console from where you launched `./xcode.sh` for
eventual errors compiling the Lisp code.
```
$ cd build-ios

$ qmake-ios ..
$ qmake-ios .. # run twice

$ ./xcode.sh
```
The script above first cross-compiles the Lisp code, then opens **Xcode**.

Please note:

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


Advanced note
-------------
For conditions during Qt event processing, a fallback restart is added at
startup (needed in e.g. Slime).

If you don't want this, define the following in `app.pro`:
```
DEFINES += NO_QT_RESTART
```
