
Prepare
-------

Please copy the app template files first:
```
$ cd ..
$ ./copy.sh 9999
```


Info
----

Simple canvas example: draw in JS, calculate in Lisp.

The QML file to be loaded is assumed to be `qml/main.qml`. This is defined in
`main.cpp` (see sources).

see also: [cistercian numerals](https://en.wikipedia.org/wiki/Cistercian_numerals)


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

