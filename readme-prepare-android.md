Install android development tools
---------------------------------

A good description of what you'll need can be found here:

[doc.qt.io/qt-5/android-getting-started](https://doc.qt.io/qt-5/android-getting-started.html)

You don't need Android Studio, so search for **command line tools** on the SDK
download page.

The NDK version to use depends on the Qt version. Qt5.15 requires `21.3`, Qt6
requires `25c`.

The latest working SDK for Qt5.15 is e.g.
`/opt/android/sdk/platforms/android-34`. If you find versions greater than 34
in your SDK platforms path, you need to move them to another directory before
compiling APKs.



Define environment variables and aliases
----------------------------------------

Put this in e.g. `~/.bashrc`:
```
  export JAVA_HOME='<path-to-jdk>'
  export ANDROID_HOME='<path-to-sdk>'
  export ANDROID_NDK_ROOT='<path-to-ndk>'
  export ANDROID_NDK_TOOLCHAIN='<path-to-ndk>/toolchains/llvm/prebuilt/linux-x86_64'
  export ECL_ANDROID='<path-to-cross-compiled-ecl>/ecl-android'

  # optional
  export ECL_ANDROID_32='<path-to-cross-compiled-ecl-32>/ecl-android'

  alias qmake-android='<path-to-qt5.15>/android/bin/qmake'
```
Add the path of the platform tools (`.../sdk/platforms-tools`) to your path, so
we can use `adb`:
```
  export PATH=$PATH:/<path-to-platform-tools>
```

(Use `source ~/.basrhc` to make new environment variables take effect
immediately in your current terminal session.)



Build cross-compiled ECL for android
------------------------------------

To build the cross-compiled ECL **aarch64**, just use the 2 scripts included in
this project. You'll need latest ECL 23.9.9.

* extract a fresh copy of the ECL sources in e.g. `~/ecl`, and rename
  `ecl-23.9.9` to `android`
* copy the 2 scripts from [platforms/android/build-ecl/](platforms/android/build-ecl/)
  to `~/ecl/android/`
* run both scripts in order
```
  ./1-make-ecl-host.sh
  ./2-make-ecl-android.sh
```
Now you should have your cross-compiled ECL under `~/ecl/android/ecl-android/`,  
and your host ECL (for cross-compiling) under `~/ecl/android/ecl-android-host/`.



Build 32bit ECL (optional)
--------------------------

For older devices you may need a 32bit version. Just use the build scripts in
[platforms/android/build-ecl/32bit/](platforms/android/build-ecl/32bit/) and
set `ECL_ANDROID_32` (see above).

Before running the scripts, you need to add this on top of file
`src/bdwgc/pthread_stop_world.c`:
```
#undef USE_TKILL_ON_ANDROID
```

For 32bit builds you need to substitute every `qmake-android ...` with:
```
qmake-android "CONFIG+=32bit" ...
```
Just use the respective `build-android/` directory for building, and remember
to do a `make clean` before running `make`.
