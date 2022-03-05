
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

Qt is (obviously) working perfectly well on Linux (and on Linux only, I dare to
say).

On macOS/iOS you'll always find some subtle bug, like a QQuickView not updating
after a property change through e.g. Slime.

In the above case you need to click on the QQuickView in order to update the
view; after that, it seems to work for subsequent property changes.
