# Installation of optional Qt5.15 directly on device

Important: this requires **SailfishOS 4.5**.

Go to this site [chumrpm.netlify.app](https://chumrpm.netlify.app/) and
download/install **sailfishos-chum-gui** (you don't need the gui, but it's
practical to see what's installed on your phone).

Important: choose **4.5.0.19** as **SailfishOS** version, this is guaranteed
to work with Qt5.15 (not all versions have all required rpms we need).

Above app will add the [chum](https://repo.sailfishos.org/obs/sailfishos:/chum/)
repo that we need here (so just run it once to have it set up).

Install the following packages:
```
$ devel-su zypper install qt-runner
$ devel-su zypper install opt-qt5-qtdeclarative-devel
$ devel-su zypper install opt-qt5-qtconnectivity-devel
$ devel-su zypper install opt-qt5-qtlocation-devel
$ devel-su zypper install opt-qt5-qtmultimedia-devel
$ devel-su zypper install opt-qt5-qtquickcontrols2-devel
$ devel-su zypper install opt-qt5-qtsensors-devel
```
If it says:
```
Problem: This request will break your system!
```
You can safely ignore those warnings, nothing will really break (see below),
so just choose:
```
Solution 3: break <name>-devel by ignoring some of its dependencies
```

Now you should be able to build/install the `lqml` executable and library.

If the compiler complains about missing headers `GLES3/*.h` and `KHR/*.h`,
extract [missing-headers.tgz](missing-headers.tgz) in `/usr/include/`.
Alternatively copy the missing headers from any android NDK.
