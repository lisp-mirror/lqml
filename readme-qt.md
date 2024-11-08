Official online installer
-------------------------

[download-qt-installer-oss](https://www.qt.io/download-qt-installer-oss)


Linux note (desktop)
--------------------

Ubuntu 22.04 users just need to install the following (assuming the build
system is already installed):

```
sudo apt install qtdeclarative5-dev qtquickcontrols2-5-dev
```


macOS note (desktop)
--------------------

It's trivial to install Qt5.15/desktop with **brew**:
```
brew install qt@5
```
Just create an alias for **qmake**, like:
```
alias qmake='/usr/local/Cellar/qt@5/5.15.8_3/bin/qmake'
```
