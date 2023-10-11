# Manual installation of Qt5.15 rpms directly on device

Important: this requires **SailfishOS 4.5**.

Please note that this is not a straightforward way to install, and may turn out
being tedious (I'm not a SailfishOS expert and have very limited experience).

To download all rpms (on desktop Linux), set architecture (default: `armv7hl`)
in file `download-rpms.lisp` and run it in ECL:
```
$ ecl -shell download-rpms.lisp # will download to 'rpms/<arch>/'
```


## Installing all base rpms

Copy the rpms to device (see `sshfs`) and install with (beware that this will
give some unmet dependencies):
```
$ cd rpms/<arch>
$ ./install.sh
```

On missing SailfishOS lib dependencies, search for package providing the lib
with:
```
$ zypper search <name> # omit 'lib' if not found
```
and install with:
```
$ devel-su zypper install <name from search>
```
Repeat until above `install.sh` is successful.


## Installing devel rpms

First ensure that `pkgconfig` is already installed:
```
$ devel-su zypper install pkgconfig
```
All `*-devel` packages must be extracted and copied manually, because they all
have missing `pkgconfig(...)` dependencies (which we won't need anyway).

To do the above, just run the following scipt:
```
$ cd ../<arch>-devel/
$ ./extract-and-copy.sh
```
Now you should be able to compile lqml, using this qmake: `/opt/qt5/bin/qmake`.
