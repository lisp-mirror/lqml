
Description
-----------

A lightweight ECL based QML-only binding to Qt5/Qt6.

This small project aims to simplify all the steps needed for building
cross-platform apps. The same sources can be used to build executables for both
desktop (Linux/macOS/Windows) and mobile (android/iOS).


QML auto reload
---------------

A new feature is auto reloading of QML files after saving any changes. This
works both on the desktop and on mobile.

As a concrete example, you may have running your app on the desktop, and have
both an android mobile device plus an iOS mobile device pointing to the IP of
the desktop. Now you will see any change to QML on all 3 screens
simultaneously.

This even works (with some limitations, and only in this
[advanced example](examples/advanced-qml-auto-reload/)) at QML file level,
which means: only the QML file currently edited is reloaded, preserving the
state of all other QML files, and -- more importantly -- the current view in
case of nested page structures.


License
-------

Both ECL and Qt5/Qt6 are **LGPL** (being Qt6 a special case to consider).

LQML is **BSD** 0 clause.


Tested
------

The examples are only tested with **Qt5.15**. The binding also works with
**Qt6**, but would need some adaption of QML in most examples. It's recommended
to use the new Qt online installer (see [readme-qt](readme-qt.md)), where
you can choose to install different Qt versions side by side, sharing the same
Qt Creator.


TODO
----

* port to CMake (?)


macOS note
----------

Qt works (obviously) perfectly well on Linux.

On macOS instead, I found this a little annoying bug: after a QML property
change in Slime, the QQuickView is not updating. So, for a visual update you
need to click on the view; but it seems to work for subsequent property
changes.


Windows note
------------

The Windows version is only meant to run on the desktop, using the (free) MSVC
compiler. Please see [readme-windows](readme-windows.md) for details.


Known issues
------------

* on **iOS**, functions `read-sequence`, `read-char` etc. don't update
  `file-position` (ECL bug?); for a workaround see hack
  [here](examples/cl-repl/lisp/upload-download.lisp)
