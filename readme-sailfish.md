Building ECL + LQML
-------------------

*Please note that using the optional Qt5.15 libs (otherwise lqml would not
compile) means that we won't have native look & feel, and integration with
SailfishOS is not optimal, but this seems the only feasible way to run it,
because backporting everything to Qt5.6 (from native SailfishOS) is not a
realistic option.*

If you run Linux on your desktop, the simplest way to build and develop is just
doing it directly on the device (only this route is described here). No bloated
SDK or similar is needed.

After connecting your Sailfish device via USB, open 2 console tabs on Linux:

### Tab 1: shell
```
$ ssh -L4005:127.0.0.1:4005 defaultuser@192.168.2.15
```
You need to manually do `source ~/.bashrc` for e.g. your personal aliases to
work.

### Tab 2: file access
```
$ mkdir ~/phone
$ sshfs defaultuser@192.168.2.15: ~/phone
```

First try to compile/install ECL (tab 1) just like you would do on the desktop.

Then follow the description
[qt5.15-rpm-installation](platforms/sailfish/qt5.15-installation.md).

Add an alias in `~/.bashrc` on the device:
```
alias qmake5.15=/opt/qt5/bin/qmake
```

Run `qt-runner` without arguments to check the following settings:

- "Override DPI": set to 2/3 of max value, e.g. 458 * 2/3 = 305 (on my phone)
- select "Reduce window when keyboard is shown"
- set variable `QT_QUICK_CONTROLS_STYLE` to something neutral like `default`
  (the original `breeze` style setting doesn't work with lqml)
- apply changes (top right)

Now you should be able to build/install lqml using `qmake` from above
installation. Note that you always need to start the apps using `qt-runner`:
```
$ qt-runner lqml run.lisp
```

For your final apps, just compile them in `build/`, rename `app` accordingly
and copy it under e.g. `/usr/local/bin/`. Since the executables contain all
resources, nothing else needs to be installed.

If you want to run a compiled but not installed app, you need to pass the path:
```
$ qt-runner ./my-app
```


Developing with Slime directly on device
----------------------------------------

Using the 2 console tabs as described above, you can start Swank in tab 1:
```
$ qt-runner lqml run.lisp -slime
```
Add `-auto` for QML auto reload (just like on the desktop).

In tab 2 switch to a lqml example directory on device, open any Lisp file in
Emacs and connect:
```
:slime-connect RET RET
```


Desktop icon
------------

For the desktop icon see example for `cl-repl` in
[readme](platforms/sailfish/desktop-icon-example/readme.md) and files in
`platforms/sailfish/desktop-icon-example`.


Tips
----

You should always kill `sshfs` (desktop Linux) after you are done using it:
```
$ pkill sshfs
```


Alternative approach
--------------------

The Sailfish Qt5.15 project (which inspired this) can be found
[here](https://github.com/sailfishos-chum/qt5/wiki/Getting-Started).

It uses the official Sailfish SDK for development.
