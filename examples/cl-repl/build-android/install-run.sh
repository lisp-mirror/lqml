# install/update (keeps app data)
adb install -r android-build/*.apk
adb shell am start -n org.eql5.android.repl/org.qtproject.qt5.android.bindings.QtActivity # Qt5
