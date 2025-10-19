
Info
----

This is a debug dialog (taken from example cl-repl) to be integrated in your
app.

So, if you merge this example with your app, the mobile app will not crash on
an eventual runtime error: an interactive debug dialog will be shown instead.

This is especially helpful on android, where Lisp issues are hard to debug once
the app is installed.


Prepare
-------

* extract `local-projects/lqml-debug.tgz` under `~/quicklisp/local-projects/`
* add `lqml-debug` as your very first dependency in your `app.asd`
* modify your `main.qml` as can be seen in `examples/`
* comment out evtl. present `eval.lisp` (needed for repl) and `Ext.Repl {}`


Important note
--------------

You can't have both this **debug-ui** and the simple **repl** (`Repl.qml`) in
the same app, because `Repl.qml` depends on `eval.lisp` which would replace
the stream/buffer setup of **debug-ui**.
