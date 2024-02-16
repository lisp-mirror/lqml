
**android only**


Why
---

Not strictly necessary, but this enables faster BLE scan mode, like in the
official 'meshtastic' android app.


HowTo
-----

Install the Qt5.15 sources and do:
```
$ cd <qt-sources>/qtconnectivity/src/android/bluetooth/src/org/qtproject/qt5/android/bluetooth
```
* apply patch `java.diff`
```
$ cd <qt-sources>/qtconnectivity/src/android/bluetooth
$ qmake-android bluetooth.pro
$ make
$ chmod +x ~/Qt/5.15.2/android/jar/QtAndroidBluetooth.jar
```

