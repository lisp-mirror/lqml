*This example is outdated and not really useful anymore, sorry.*


Try it
------

You can download an APK (android devices) of this example from DropBox:
[CLOG demo](https://www.dropbox.com/s/h5wy57niq4g12ec/CLOG-demo.apk?dl=0).



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
the `lqml` executable. Additionally you need to clear the ECL cache of CLOG
(see `~/.cache/common-lisp/ecl-...`), because the CLOG desktop version for LQML
is different from the standard version (see `#+mobile` in CLOG fork, which is
also needed on the desktop, if used with LQML).



Info
----

This shows how to run a CLOG app locally on mobile. It uses two different
approaches, depending on the OS:

1. direct calls to the browser to run JS, and a small hack to call back to CLOG
   on browser events

2. a simple local websocket-server; this is needed on iOS, where the above
   approach doesn't work

The webview is the native one of the mobile device, which has some
restrictions: it can't overlap with QML items, and things like swiping don't
work.

On the second page you can see a log of CLOG messages, and start Swank from the
REPL. Just connect from Slime using the WiFi IP of the mobile device.
