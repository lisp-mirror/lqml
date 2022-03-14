Define environment variable and alias
-------------------------------------

Put this in e.g. `~/.profile`:
```
  export ECL_IOS='<path-to-cross-compiled-ecl>/ecl-ios'

  alias qmake-ios='<path-to-qt5.15>/ios/bin/qmake'
```
(Use `source ~/.profile` to make new environment variables take effect
immediately in your current terminal session.)



Build cross-compiled ECL for iOS
--------------------------------

As of March 2022, please use latest ECL from development branch.

* extract a fresh copy of the ECL sources in e.g. `~/ecl`, and rename
  `ecl-21.2.1` to `ios`
* copy the 2 scripts from [platforms/ios/build-ecl/](platforms/ios/build-ecl/)
  to `~/ecl/ios/`
* run first script
```
  ./1-make-ecl-host.sh
```
Edit `src/c/unixsys.d` and search for `HAVE_SYSTEM`; right before the function
definition inside which it occurs, put this line:
```
  #undef HAVE_SYSTEM
```
Edit `src/c/thread/process.d`, search for `pthread_attr_init` (around line 588)
and add the following below that line:
```
  pthread_attr_setstacksize(&pthreadattr, 2 * 236 * 4096); // double default size
```
* run second script
```
  ./2-make-ecl-ios.sh
```

Now you should have your cross-compiled ECL under `~/ecl/ios/ecl-ios/`, and
your host ECL (for cross-compiling) under `~/ecl/ios/ecl-ios-host/`.



Build for iOS Simulator (optional)
----------------------------------

Note: only tested on **Intel**.

For this to work, you will need to upgrade your **gmp** library to the latest
version (tested with gmp 6.2.1). Just substitute it in `src/gmp/`:

* download [gmp](https://gmplib.org/download/gmp/gmp-6.2.1.tar.xz)

* extract it with `tar -xvf gmp-6.2.1.tar.xz` and replace the version in `src/gmp/`

* run second script again, passing **sim**
```
  ./2-make-ecl-ios.sh sim
```
This will first compile for the simulator, then combine both versions to
universal binaries.
