Example desktop icon for cl-repl
--------------------------------

On the device:

- after compiling (and stripping) it, put `cl-repl` executable in `/usr/bin/`
- copy files under `usr` in `/usr`:
```
$ devel-su cp -r usr/* /usr/
```
The icon is just a generic Sailfish icon named `lqml.png`.

The `cl-repl.desktop` file is an edited copy of `qt-runner.desktop`.
