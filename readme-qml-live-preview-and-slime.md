
QML Preview and Slime
---------------------

(LQML >= 22.2.2, see `lqml -v`)

This shows how to use **QML Preview** (live QML updates while you are typing).

First you need to apply 2 small changes:

* edit `lisp/main.lisp` of an example, and add this line at the end:
```
(load "~/slime/lqml-start-swank.lisp")
```

* edit `app.pro` and change `DEFINES` to:
```
DEFINES += INI_LISP SWANK
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
should already be running, see 'Application Ouput' at the bottom of Qt
Creator).

[Screenshot](screenshots/qml-live-preview-and-slime.jpg)
