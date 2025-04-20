*This example has been updated in 2025 to CLOG version 2.2.*


Try it
------

You can download an APK (android devices) of this example from DropBox:
[CLOG demo](https://www.dropbox.com/s/h5wy57niq4g12ec/CLOG-demo.apk?dl=0).

**Please note**: startup time on mobile is greatly improved here, by simply
skipping time consuming crypto initialization, not needed for a local
connection, like in this example.



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
[CLOG for mobile](https://gitlab.com/eql/clog-for-mobile/-/blob/main/clog-2.2.tgz).

If you want to run this example on the desktop (only tested with Qt6), you need
to uncomment the Qt WebEngine dependency in [../../src/lqml.pro](../../src/lqml.pro)
and rebuild the `lqml` executable. Additionally you need to clear the ECL cache
of CLOG (see `~/.cache/common-lisp/ecl-...`), because the CLOG desktop version
for LQML is different from the standard version (see `#+mobile` in CLOG fork,
which is also needed on the desktop, if used with LQML).



Info
----

This shows how to run a CLOG app locally on mobile. It uses a simple local
**websocket-server**.

For android, please see [AndroidManifest.xml](platforms/android/AndroidManifest.xml)
for the `@xml/network_security_config` file needed to allow a local connection.

The webview is the native one of the mobile device, which has some
restrictions: it can't overlap with QML items, and things like swiping don't
work.

On the second page you can see a log of CLOG messages, and start Swank from the
REPL. Just connect from Slime using the WiFi IP of the mobile device.
