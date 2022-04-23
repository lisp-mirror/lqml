
Requirements
------------

This only describes the MSVC build.

* install the free Visual Studio 2019 Community edition (in case of Qt5)

* install the Windows SDK (for C header files etc.)

* install either Qt5.15 (recommended) or Qt6 (will need some adaption of QML in
  the examples) from the online installer, see [readme-qt](readme-qt.md)

To be able to run everything from the command line, you need to set both
the MSVC and the Qt environment variables like so:

In the Windows start menu, search for **qt** and launch the command prompt
window. This will set the Qt environment variables. Additionally you need the
MSVC environment variables. The manual steps for this are something like:
```
cd C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build 

vcvars64.bat
```



Compile ECL
-----------

Extract the ECL sources in e.g. `C:\ecl`. Edit file `C:\ecl\msvc\Makefile` and
change `# ECL_WIN64 =` to `ECL_WIN64 = 1`.

Run `nmake` followed by `nmake install`.



Quicklisp/ASDF notes
--------------------

To avoid confusion, please note:

* **don't** install Quicklisp using `(require :ecl-quicklisp)`; just install it
  the usual way, see [Quicklisp beta](https://www.quicklisp.org/beta/)

* put this as first line in your `C:\Users\name\.eclrc`: `(require :cmp)` in
  order to always use the **C compiler** (instead of the default bytecodes
  compiler)



Build LQML
----------

If you installed ECL in a different place than above, you need to adapt the
paths in [src/windows.pri](src/windows.pri).

Make sure you have both ECL and (future) LQML in your path:
```
set PATH=%PATH%;C:\ecl\msvc\package;C:\lqml\src\build\release
```
Switch to `C:\lqml\src\build` and run:
```
qmake ..\lqml.pro
nmake

qmake ..\lqml-lib.pro
nmake
```

Now you should be able to run the examples and build them as executables.
