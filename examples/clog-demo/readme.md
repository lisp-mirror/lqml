
Prepare
-------

Please copy the app template files first:
```
$ cd ..
$ ./copy.sh clog-demo
```

See also [../../slime/src/readme-sources](../../slime/src/readme-sources.md)
for installing the Slime sources where this example can find them.

**Important**: you need to put this fork of CLOG in your
`~/quicklisp/local-projects/` directory:
[CLOG for mobile](https://github.com/pls153/clog).

If you want to run this example on the desktop, you need to uncomment the Qt
WebEngine dependency in [../../src/lqml.pro](../../src/lqml.pro) and rebuild
the `lqml` executable.



Info
----

This shows how to run CLOG on mobile. It uses the native web-view on mobile,
which has some restrictions: to see page 2 of the QML UI, you need to swipe
at the bottom (where the 'Reload' button is), because swiping the native
web-view won't work.

On the second page you can see a log of CLOG messages, and start Swank from the
REPL.
