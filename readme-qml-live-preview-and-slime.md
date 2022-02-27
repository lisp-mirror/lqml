
QML Preview and Slime
---------------------

(LQML >= 22.2.2, see `lqml -v`)

If you want to be impressed (I certainly was when I first tried it), do the
following:

* edit `lisp/main.lisp` of an example, and add this line at the end:
```
(load "~/slime/lqml-start-swank.lisp")
```

* edit `app.pro` and change `DEFINES` to:
```
DEFINES += INI_LISP SWANK
```
* open `app.pro` in **Qt Creator**, and in 'Build Settings' choose `build/`
  from the example as 'Build directory'

* in the 'Edit' view of Qt Creator choose 'Build / QML Preview' (after a
  'Build / Clean')

Now the example should compile and start. You may now select any QML file from
the tree view on the left, and any change to QML will be shown immediately in
the running app, while you are typing!

Additionally you may now also run `M-x slime-connect` from Emacs (the Swank
server should already be running, see 'Application Ouput' at the bottom of Qt
Creator).

[Screenshot](screenshots/qml-live-preview-and-slime.jpg)
