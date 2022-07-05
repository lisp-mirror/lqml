
Note
----

This will only work with Qt 5.15 (customized AndroidManifest.xml file) and
requires WearOS 2 or later.

It is assumed that you already enabled the developer settings on your watch.

--

Every Lisp file under `lisp/` and every qml/image/font/whatever file under
`qml/` is added **automatically** to your Qt project file (both for re-compile
and adding to resources included in the executable).

So, you only need to **manually** care about the usual ASDF project files in
`app.asd`.

But -- *of course* -- you still need to run the respective **qmake** command
every time you add new files to the project, because the automation is all
defined in `app.pro`.


Build 32bit android APK
-----------------------

First you need to connect your device through WiFi. Please set your watch IP
address in [build-android/connect.sh](build-android/connect.sh) and run:
```
$ ./build-android/connect.sh
```

You probably want to build a **32bit** version, because most watches are 32bit.

For this you first need to build a 32bit version of the LQML library, see
[readme-prepare-android.md](../../readme-prepare-android.md).

Now you can build the app:
```
$ cd build-android

$ qmake-android .. # "CONFIG+=32bit" can be omitted here: already defined in app.pro
$ make apk

$ ./install-run.sh
```
Log note: for showing only your own messages, see `log.sh`.



Important note (Qt bug)
-----------------------

After editing [AndroidManifest.xml](platforms/android/AndroidManifest.xml)
in **Qt Creator** you need to add this inside `activity`:
```
android:theme="@android:style/Theme.DeviceDefault"
```
If you omit the above setting, the app will not start and just show an error
message instead.

