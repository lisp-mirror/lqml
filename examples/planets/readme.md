
Prepare
-------

Please copy the app template files first:
```
$ cd ..
$ ./copy.sh planets
```


Info
----

This shows how to populate a QML item model from Lisp.

The QML code is a modified version of an example found in the Qt6 QML book. The
images are taken from the qt3d planets-qml example.



Run
---

```
lqml run.lisp
```
Optionally pass `-slime` to start a Swank server, and connect from Emacs with
`M-x slime-connect`.

Closing the window quits the app. If you try to kill it with `ctrl-c`, you need
an additional `ctrl-d` to exit from ECL. To quit from Slime, do `(qq)` which is
short for `(qquit)`.

