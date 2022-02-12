
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

N.B: Before building the app, go to Build Settings / Build Options and set
**Enable Bitcode** to **No**.

The simulator will not work here, because we didn't cross-compile ECL and
the app code for the simulator.

