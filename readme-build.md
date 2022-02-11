
Build executable
----------------

Currently still using qmake, will eventually be ported to CMake.

Please make sure you have both latest **ECL** (from development branch) and
either **Qt5.15** or **Qt6** installed (see [readme-qt](readme-qt.md)).

```
$ cd src/build

$ qmake ../lqml.pro
$ make -j4
$ sudo make install
```


Build library
-------------

To build the static library needed for building your own apps, do:

* desktop
```
$ cd src/build

$ qmake ../lqml-lib.pro
$ make
```
* android (note separate build directory)
```
$ cd src/build-android

$ qmake-android ../lqml-lib.pro
$ make
```
* ios (note separate build directory)
```
$ cd src/build-ios

$ qmake-ios ../lqml-lib.pro
$ make
```
The library files can be found under `platforms/<os>/lib/`

