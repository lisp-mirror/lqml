This is here for testing SSL on android, both on **Lisp** and on **Qt** side.

After doing `make apk`, please ensure files
```
libssl.so
libcrypto.so
```
are present in directory
```
build-android/android-build/libs/arm64-v8a/
```
(that is, they should have been copied automatically to above place)

The official Qt OpenSSL android versions can be downloaded from
[KDAB android openssl](https://github.com/KDAB/android_openssl).

Uninstall any app called 'app' (which you might have previously installed)
before installing this one.

When running the app, it should first display "downloading...", and after a few
seconds (if the android device is connected to the internet), it should display
"OK" (Lisp side of SSL); it should also display a Lisp logo, downloaded through
https (Qt side of SSL).
