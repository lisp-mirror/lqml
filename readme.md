
Description
-----------

A lightweight ECL based QML-only binding to Qt5/Qt6.

This small project aims to simplify all the steps needed for building
cross-platform apps. The same sources can be used to build executables for both
desktop (Linux/macOS) and mobile (android/iOS).


License
-------

ECL and Qt5/Qt6 are LGPL.
LQML can be considered public domain.


Tested
------

Only tested with **Qt5.15** and latest **Qt6**. It's recommended to use the new
Qt online installer (see [doc/get-qt6](doc/get-qt6.md)), where you can choose
to install different Qt versions side by side, sharing the same Qt Creator.

The **mobile** part is currently only tested with **Qt5.15**, because the Qt6
port still lacks significant parts of mobile (as of Qt6.2).


TODO
----

* add sokoban example
* add CL REPL example
* add Windows platform
* port to CMake


macOS note
----------

Qt works (obviously) perfectly well on Linux.

On macOS instead, I found this a little annoying bug: after a QML property
change in Slime, the QQuickView is not updating. So, for a visual update you
need to click on the view; but it seems to work for subsequent property
changes.
