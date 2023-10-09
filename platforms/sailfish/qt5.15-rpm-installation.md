Manual installation of Qt5.15 rpms directly on device
-----------------------------------------------------

Please note that this is not a straightforward way to install, and may turn out
being tedious (I'm not a SailfishOS expert and have very limited experience).

(for **armv7hl**, replace with **aarch64** for 64bit)

Download from repo (see file list at bottom):
```
https://repo.sailfishos.org/obs/sailfishos:/chum/4.5.0.19_armv7hl/armv7hl/
```

Download all files in one directory and install with:
```
$ devel-su rpm -U --force *.rpm
```

On missing SailfishOS lib dependencies, search for package providing the lib
with:
```
$ zypper search <name>
```
and install with:
```
$ devel-su zypper install <name>
```

To force package install even with missing dependencies - may occur with rpms
that require e.g. `pkgconfig(...)`, even with standard `pkgconfig` already
installed - you can extract the rpm locally with:
```
$ rpm2cpio <rpm> | cpio -idmv
```
and copy the extracted files under `/opt/` and `/usr/`:
```
$ devel-su cp -r opt/* /opt/
$ devel-su cp -r usr/* /usr/
```
File list to download:
```
opt-kf5-karchive-5.108.0-1.5.2.jolla.armv7hl.rpm
opt-kf5-kauth-5.108.0-1.4.1.jolla.armv7hl.rpm
opt-kf5-kbookmarks-5.108.0-1.4.1.jolla.armv7hl.rpm
opt-kf5-kcodecs-5.108.0-1.5.1.jolla.armv7hl.rpm
opt-kf5-kcompletion-5.108.0-1.4.1.jolla.armv7hl.rpm
opt-kf5-kconfig-5.108.0-1.5.1.jolla.armv7hl.rpm
opt-kf5-kconfig-core-5.108.0-1.5.1.jolla.armv7hl.rpm
opt-kf5-kconfig-gui-5.108.0-1.5.1.jolla.armv7hl.rpm
opt-kf5-kconfigwidgets-5.108.0-1.5.1.jolla.armv7hl.rpm
opt-kf5-kcoreaddons-5.108.0-1.5.1.jolla.armv7hl.rpm
opt-kf5-kcrash-5.108.0-1.4.1.jolla.armv7hl.rpm
opt-kf5-kdbusaddons-5.108.0-1.5.1.jolla.armv7hl.rpm
opt-kf5-kguiaddons-5.108.0-1.5.1.jolla.armv7hl.rpm
opt-kf5-ki18n-5.108.0-1.5.1.jolla.armv7hl.rpm
opt-kf5-kiconthemes-5.108.0-1.5.1.jolla.armv7hl.rpm
opt-kf5-kio-core-5.108.0-1.7.1.jolla.armv7hl.rpm
opt-kf5-kio-core-libs-5.108.0-1.7.1.jolla.armv7hl.rpm
opt-kf5-kio-file-widgets-5.108.0-1.7.1.jolla.armv7hl.rpm
opt-kf5-kio-gui-5.108.0-1.7.1.jolla.armv7hl.rpm
opt-kf5-kio-widgets-5.108.0-1.7.1.jolla.armv7hl.rpm
opt-kf5-kio-widgets-libs-5.108.0-1.7.1.jolla.armv7hl.rpm
opt-kf5-kirigami2-5.108.0-1.6.1.jolla.armv7hl.rpm
opt-kf5-kitemviews-5.108.0-1.5.1.jolla.armv7hl.rpm
opt-kf5-kjobwidgets-5.108.0-1.4.1.jolla.armv7hl.rpm
opt-kf5-knotifications-5.108.0-1.5.1.jolla.armv7hl.rpm
opt-kf5-kquickcharts-5.108.0-1.5.1.jolla.armv7hl.rpm
opt-kf5-kservice-5.108.0-1.4.1.jolla.armv7hl.rpm
opt-kf5-kwallet-5.108.0-1.4.1.jolla.armv7hl.rpm
opt-kf5-kwallet-libs-5.108.0-1.4.1.jolla.armv7hl.rpm
opt-kf5-kwidgetsaddons-5.108.0-1.5.1.jolla.armv7hl.rpm
opt-kf5-kwindowsystem-5.108.0-1.5.1.jolla.armv7hl.rpm
opt-kf5-kxmlgui-5.108.0-1.4.1.jolla.armv7hl.rpm
opt-kf5-solid-5.108.0-1.4.1.jolla.armv7hl.rpm
opt-plasma-integration-5.27.4\+git1-1.2.7.jolla.armv7hl.rpm
opt-qt5-qtbase-5.15.10\+kde137-1.3.1.jolla.armv7hl.rpm
opt-qt5-qtbase-common-5.15.10\+kde137-1.3.1.jolla.noarch.rpm
opt-qt5-qtbase-devel-5.15.10+kde137-1.3.1.jolla.armv7hl.rpm
opt-qt5-qtbase-gui-5.15.10\+kde137-1.3.1.jolla.armv7hl.rpm
opt-qt5-qtconnectivity-5.15.10\+kde4-1.3.1.jolla.armv7hl.rpm
opt-qt5-qtconnectivity-devel-5.15.10+kde4-1.3.1.jolla.armv7hl.rpm 
opt-qt5-qtdeclarative-5.15.10\+kde29-1.3.1.jolla.armv7hl.rpm
opt-qt5-qtdeclarative-devel-5.15.10+kde29-1.3.1.jolla.armv7hl.rpm
opt-qt5-qtdeclarative-tools-5.15.10+kde29-1.3.1.jolla.armv7hl.rpm
opt-qt5-qtgraphicaleffects-5.15.10-1.3.1.jolla.armv7hl.rpm
opt-qt5-qtimageformats-5.15.10+kde9-1.3.1.jolla.armv7hl.rpm
opt-qt5-qtlocation-5.15.10\+kde4-1.5.1.jolla.armv7hl.rpm
opt-qt5-qtlocation-devel-5.15.10+kde4-1.5.1.jolla.armv7hl.rpm
opt-qt5-qtmultimedia-5.15.10+kde3-1.3.1.jolla.armv7hl.rpm
opt-qt5-qtmultimedia-devel-5.15.10+kde3-1.3.1.jolla.armv7hl.rpm
opt-qt5-qtquickcontrols-5.15.10-1.3.1.jolla.armv7hl.rpm
opt-qt5-qtquickcontrols2-5.15.10\+kde6-1.3.1.jolla.armv7hl.rpm
opt-qt5-qtquickcontrols2-devel-5.15.10+kde6-1.3.1.jolla.armv7hl.rpm
opt-qt5-qtsensors-5.15.10-1.3.1.jolla.armv7hl.rpm
opt-qt5-qtsvg-5.15.10\+kde8-1.3.1.jolla.armv7hl.rpm
opt-qt5-qtwayland-5.15.10\+kde52-1.3.1.jolla.armv7hl.rpm
opt-qt5-qtwebchannel-5.15.10\+kde3-1.3.1.jolla.armv7hl.rpm
opt-qt5-qtwebengine-5.15.14-1.2307312104.armv7hl.rpm
opt-qt5-qtwebsockets-5.15.10+kde2-1.3.1.jolla.armv7hl.rpm
opt-qt5-qtwebview-5.15.10-1.3.2.jolla.armv7hl.rpm
opt-qt5-rpm-macros-5.15.10-1.4.1.jolla.noarch.rpm
opt-qt5-sfos-maliit-platforminputcontext-1.0.1-1.1.4.jolla.armv7hl.rpm
qqc2-breeze-style-5.27.4\+git1-1.2.8.jolla.armv7hl.rpm
qt-runner-0.4.0-1.6.1.jolla.armv7hl.rpm
```
