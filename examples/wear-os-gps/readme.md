
Prepare
-------

Please copy the app template files first:
```
$ cd ..
$ ./copy.sh wear-os-gps
```


Info
----

This is a practical example of displaying both the speed and the whole distance
of e.g. a canoe session (only meant for constant altitude values). You probably
need to adapt the maximum speed value (km/h) to your personal needs, see
settings (swipe up).

An important feature (android only) is keeping the display always on
(implemented with the Qt JNI interface). But this also consumes more battery,
so you can switch it off in the settings.

The data is automatically logged, and can be accessed with e.g.
**Device File Explorer** from Android Studio (see Help / Find Action...).

The path of the log files is:
```
/data/data/org.qtproject.example.gps/files/
```

You can then put those log files in directory [kml/logs/](kml/logs/) and run
```
cd kml

ecl -shell kml.lisp
```
This will generate **kmz** files of all log files, which can then be viewed
using **Google Earth** (the free desktop app).

A simple **Kalman** filter is used for the necessary GPS data smoothing.

The UI uses a `CircularGauge` for displaying the speed (by default, an average
of the latest 5 seconds). Additionally it shows the whole distance and the GPS
accuracy in meters.



Run (desktop demo)
------------------
```
lqml run.lisp
```
Optionally pass `-slime` to start a Swank server, and connect from Emacs with
`M-x slime-connect`.

During development you can pass `-auto`, which will releoad all QML files after
you made a change to any of them and saved it. For re-initialization after
reloading, file `lisp/qml-reload/on-reloaded` will be loaded.

Closing the window quits the app. If you try to kill it with `ctrl-c`, you need
an additional `ctrl-d` to exit from ECL. To quit from Slime, do `(qq)` which is
short for `(qquit)`.



Note
----

Even though this example is made for a WearOS watch, it can also be run on any
android/iOS phone (preferably in landscape orientation).
