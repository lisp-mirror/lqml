
**android only**


Why
---

This is needed on android because a mobile device which doesn't move only
receives one (or maybe two) GPS updates before stopping the updates.

Since we have a long startup time with this app, we would completely lose
the first GPS updates using the convenient QML wrapper `PositionSource`,
hence not being able to communicate our position to the radio device.

The only workaround seems to be to directly access the Java `Location`
functions.


HowTo
-----

Just copy `QtActivity.java` to path in `path.txt` (for Qt 5.15);
alternatively apply patch `git.diff`.

See also [cpp/qt.cpp](../cpp/qt.cpp).
