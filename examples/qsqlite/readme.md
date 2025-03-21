
Info
----

This is a simple example of:

* using the **qsqlite** (Qt specific SQLite DB) library directly from Lisp
* providing a custom image provider, so we can directly load images in QML from
  an SQL database

The qsqlite library that comes with Qt has the advantage of being pulled in
automatically as a dependency, and behaving exactly the same, no matter what OS
is used. This is especially convenient on mobile.

If you run the example with:
```
$ lqml run.lisp -slime
```
you can then try to change the image source from the REPL:
```
(in-package :app)

(q> |source| ui:*logo* "image://db/logo-256")
```
This will load the image directly from the database.


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

