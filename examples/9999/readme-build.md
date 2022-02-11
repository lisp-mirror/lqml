
Run desktop
-----------

```
$ lqml run.lisp
```


Build desktop app
-----------------

```
$ cd build

$ qmake ../app.pro
$ make
```


Build android APK
-----------------

```
$ cd build-android

$ qmake-android ../app.pro
$ make apk

$ adb install -r android-build/*.apk
```

