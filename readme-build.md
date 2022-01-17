
Build
-----

Currently still using qmake, will be ported to CMake

* make sure you have both **ECL** and **Qt6** installed
* make sure to use `qmake` from Qt6

```
$ cd src
$ mkdir build
$ cd build

$ qmake ../lqml.pro
$ make -j4
$ sudo make install
```

