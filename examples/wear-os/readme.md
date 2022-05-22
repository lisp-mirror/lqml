
Prepare
-------

Please copy the app template files first:
```
$ cd ..
$ ./copy.sh wear-os
```


Info
----

This shows how to build WearOS apps (typically a 32bit watch).

It also shows how to integrate the (not yet officially supported by Qt) heart
rate sensor.

It takes the watch sensor a few seconds before heart rate data is available, so
initially a demo mode with 60 bpm is shown. The red border will change to green
as soon as real data is arriving.



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



...and the Apple Watch?
-----------------------

If you were wondering: both ECL and Qt run almost everywhere, but there **are**
exceptions. So, neither ECL (because of some missing OS API), nor QML (only Qt
Core would run) can currently be used on the Apple Watch.

So, just use **SwiftUI** there.
