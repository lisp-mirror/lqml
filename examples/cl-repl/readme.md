
Prepare
-------

Please copy the app template files first:
```
$ cd ..
$ ./copy.sh cl-repl
```

See also [../../slime/src/readme-sources](../../slime/src/readme-sources.md) for
installing the Slime sources where this example can find them.

**Important**: you need to patch library `:zip` from Quicklisp, so copy it in
your `~/quicklisp/local-projects/` directory, and apply this
[patch/zip.diff](patch/zip.diff).



Info
----

This is a port of the eql5-android/eql5-ios repl example, with the difference
that this version runs both on desktop and mobile.

Since syntax highlighting etc. requires some Qt code, you'll need to build the
small plugin in [cpp](cpp/) if you want to run this on the desktop.

**iOS** note: please see [qt-ios-hack](qt-ios-hack/) for the required hack to
disable the clipboard menu, otherwise it won't work on iOS.



Quicklisp note
--------------

On **andoid**, Quicklisp is directly downloaded and installed.

On **iOS** the above method would crash ECL; this seems to be caused by the
limited stack size on iOS.

So, in order to use Quicklisp on iOS, you need to bring it yourself, which
means:

* install a fresh copy of Quicklisp on some desktop device, and copy over the
  whole directory `quicklisp` under `platforms/ios/assets/Library/` of this
  example



QML auto reload on mobile
-------------------------

If you compile for mobile, it will ask for the **Wifi IP** of your desktop
computer (currently hard coded into the app). If you just hit RET, auto
reloading will be disabled.

After installing and launching the app, just run this script from your example
directory:
```
./web-server.sh
```
It requires Python 3 and the cgi module, which are probably already installed
on your computer.

You may now edit any QML file on the desktop computer, and upon saving, all of
QML will be reloaded automatically. After reloading, the following file will be
loaded for eventual re-initialization on Lisp side:
```
lisp/qml-reload/on-reloaded.lisp
```
For **android**, in order to see the debug output of eventual QML errors, you
need to run `./log.sh` in your `build-android/` directory.

If you don't want auto reload enabled, comment out `auto-reload-qml` in
`lisp/main.lisp`.

Both desktop and mobile auto reload can also be run **simultaneously**, since
they share the QML source files. Any number of mobile devices may be connected,
if they are in the same WiFi and point to the same desktop IP.



File exchange over WiFi (mobile only)
-------------------------------------

A simple web-server is integrated here for both downloading saved files to a
desktop computer, or uploading them to the mobile device.

To start the web-server, enter `:w` in the command line. It will point to the
`[Home]` directory.

### Upload

Enter the IP of your mobile device in the desktop browser, using `1701` as
port (mind the trailing `/`):
```
http://192.168.1.x:1701/
```
Now you can upload either a whole directory, or a single file. The files will
be stored in `[Home]/uploads/`. Note that different browsers may behave
differently when uploading (recursive) directories (seems to work best with
either Firefox or Chromium based browsers).

You may also upload a zip file, which can then be unzipped using:
```
(unzip "uploads/all.zip" "examples/") ; android

(unzip "uploads/all.zip" "../Documents/examples/") ; iOS
```

### Download

First create a `*.zip` file like so:
```
(zip "all.zip" "examples/") ; android

(zip "all.zip" "../Documents/examples/") ; iOS
```
Note that the zip file will not contain the passed directory name (this is how
the zip library from Quicklisp is implemented).

Now you just need to enter the path in your desktop browser, something like:
```
http://192.168.1.x:1701/all.zip
```



iOS note (smart quotes)
-----------------------

Smart quotation marks can only be disabled globally for the whole app, putting
this in the iOS section of `app.pro`:
```
DEFINES += DISABLE_SMART_QUOTES
```



Important notes for mobile
--------------------------

Please remember that installing a new version of your app on mobile will
**keep all app data** present on the device.

So, if you changed e.g. your Slime version under `platforms/.../assets/`, an
update will not replace those files, because they have been copied from the
assets on the very first start of the app.

A simple way to guarantee a clean install is simply uninstalling the app first,
both on the device and on the emulator (android) or simulator (iOS).

Alternatively call `(copy-all-asset-files)` manually (after installing an
update). This function is called automatically only on the very first app
startup.

If your local web server doesn't seem to work, please check your firewall
settings.
