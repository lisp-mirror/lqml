
Info
----

This example uses its own copy/paste menu, and there is no official way to
disable the iOS menu within QML. Since this would get in our way, we simply use
a small hack to make it possible to disable the menu from Lisp like so:

  (qml:disable-clipboard-menu)


HowTo
-----

* launch the Qt maintainance tool and install the sources of Qt5.15
* copy `qiostextinputoverlay.mm` from this directory to the path in `path.txt`
* switch to the above path and open `ios.pro` in Qt Creator
* select both the iOS and Simulator release versions, and choose the `ios`
  directory as build directory (see Details / Browse)
* build the lib from within Qt Creator
* close Qt Creator and run `make install`

Now you have a Qt version that allows disabling the clipboard menu on iOS.
