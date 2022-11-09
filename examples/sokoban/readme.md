
Prepare
-------

Please copy the app template files first:
```
$ cd ..
$ ./copy.sh sokoban
```


Info
----

This shows how to dynamically create/destroy QML items.

For the game logic please see [cl-sokoban](lisp/3rd-party/); so, this is just
an UI layer on top of that game.


Run
---
```
lqml run.lisp
```
Optionally pass `-slime` to start a Swank server, and connect from Emacs with
`M-x slime-connect`.

During development you can pass `-auto`, which will reload all QML files after
you made a change to any of them and saved it. For re-initialization after
reloading, file `lisp/qml-reload/on-reloaded` will be loaded.

Closing the window quits the app. If you try to kill it with `ctrl-c`, you need
an additional `ctrl-d` to exit from ECL. To quit from Slime, do `(qq)` which is
short for `(qquit)`.

