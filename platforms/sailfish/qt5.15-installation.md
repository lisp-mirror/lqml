# Installation of optional Qt5.15 directly on device

Important: this requires **SailfishOS 4.5**.

Go to this site [chumrpm.netlify.app](https://chumrpm.netlify.app/) and
download/install **sailfishos-chum-gui** (you don't need the gui, but it's
practical to see what's installed on your phone).

Important: choose **4.5.0.19** as chum-gui version, this is guaranteed
to work with Qt5.15 (not all versions have all required rpms we need).

Above app will add the [chum](https://repo.sailfishos.org/obs/sailfishos:/chum/)
repo that we need here.

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
In one occasion (opt-qtdeclarative-devel) it will say:
```
Problem: This request will break your system!
nothing provides 'pkgconfig(android-headers)' needed by the to be installed libhybris-devel
```
You can safely ignore this warning, nothing will really break, just choose:
```
Solution 3: break libhybris-devel by ignoring some of its dependencies
```
Now you should be able to build/install the `lqml` executable and library.
