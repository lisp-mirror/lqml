
Important notes
---------------

* **Lisp**: the cross-compile cache is
  `~/.cache/commmon-lisp/ecl-<target-platform>...`. Please remember to purge it
  if you want to rebuild everything after any change to your ECL version (read:
  not all changes to ECL are caught automatically).

* **Qt**: in the above case, a `make clean` will also force the recompilation
  of any Lisp code next time you do a build.

* **LQML**: any time you upgrade LQML, please purge the build directories in
  `src/`, including `.qmake.stash` in case you use a different Qt version
  (see also `src/mkdirs.sh`) and rebuild the whole library for every single
  platform.

You may find that an example won't work anymore after upgrading LQML. This is
probably due to internal changes (to the app template files), so you better
wipe off the whole example and start over.



Build executable
----------------

Currently still using qmake, will eventually be ported to CMake.

You probably want to create an alias for qmake, like:
```
alias qmake='<path-to-qt>/<compiler>/bin/qmake'
```

Please make sure you have both **ECL 23.9.9** and either **Qt5.15** or **Qt6**
installed (see [readme-qt](readme-qt.md)).

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

