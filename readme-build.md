
Important note
--------------

* **Lisp**: the cross-compile cache is `src/.cache`. Please remember to purge
  it if you want to rebuild everything after any change to your ECL version
  (read: not all changes to ECL are caught automatically).

* **Qt**: in the above case, a `make clean` will also force the recompilation
  of any Lisp code next time you do a build.

* **LQML**: any time you upgrade LQML, please purge the build directories in
  `src/` (see also `src/mkdirs.sh`) and rebuild the whole library for every
  single platform.



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

