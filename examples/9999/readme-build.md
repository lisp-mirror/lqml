
Prepare
-------

If you use Qt versions prior to 5.15, you need to adapt the QML module version
to your minor Qt version in file `qml/main.qml`.

Example: Qt5.**12** => import QtQuick 2.**12**.

The version number can be omitted in Qt6.



Run desktop
-----------
```
$ lqml run.lisp
```


Build desktop app
-----------------
```
$ cd build

$ qmake ../app.pro
$ make
```


Build android APK
-----------------
```
$ cd build-android

$ qmake-android ../app.pro
$ make apk

$ adb install -r android-build/*.apk
```


Build iOS app
-------------
```
$ cd build-ios

$ qmake-ios ../app.pro

$ ./xcode.sh
```
The script above first cross-compiles the Lisp code, then opens **Xcode**.

Please note:

* before building the app, go to Build Settings / Build Options and set
  **Enable Bitcode** to **No**

* if it complains about missing source files when first hitting the Run button,
  just hit the Run button again (and they will be generated)

* using latest Xcode, it may complain about the Legacy Build System; just go to
  File / Project Settings and select New Build System

The simulator will not work here, because we didn't cross-compile ECL and
the app code for the simulator.

