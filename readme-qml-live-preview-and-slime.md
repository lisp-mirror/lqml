
*Please note: the below may be convenient for sketching -- but during
development, the simple `-auto` option of `lqml run.lisp` may be more
appropriate, because it allows for re-initialization (calling Lisp code) after
reloading QML.*

*The above method also works on mobile, please see example `swank-server`. It
doesn't depend on Swank, and should therefore also be stable (it uses a local
trivial web server).*

*You may even run both the desktop and mobile device auto reload simultaneously
(`swank-server` example only), because they both watch/load the same QML
sources.*


QML Preview and Slime
---------------------

(LQML >= 22.2.2, see `lqml -v`)

This shows how to use **QML Preview** (live QML updates while you are typing).

* edit `lisp/main.lisp` of an example, and append this line:
```
(load "~/slime/lqml-start-swank.lisp")
```
* open `app.pro` in **Qt Creator**; under 'Projects' (menu on the left) /
  'Configure Project' choose 'Debug' and change the 'Build directory' to
  `build/` from the example

* in the 'Edit' view of Qt Creator make sure that 'Debug' is selected (menu on
  the left, near the bottom); from the menu at the top choose 'Build / **QML
  Preview**' (after a 'Build / Clean')

Now the example should compile and start. You may now select any QML file from
the tree view on the left, and any change to QML will be shown immediately in
the running app.

Additionally you may also run `M-x slime-connect` from Emacs (the Swank server
should already be running, see 'Application Output' at the bottom of Qt
Creator).

[Screenshot](screenshots/qml-live-preview-and-slime.jpg)

*So, this is certainly a nice feature of Qt Creator, but personally I prefer
the simple `lqml run.lisp -auto` variant, which doesn't need compilation, and
-- more importantly -- can run Lisp code after a QML reload.*
