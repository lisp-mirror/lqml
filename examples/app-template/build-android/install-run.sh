# install/update (keeps app data)
adb install -r android-build/*.apk

# try both
adb shell am start -n org.qtproject.example.app/org.qtproject.qt5.android.bindings.QtActivity # Qt5
adb shell am start -n org.qtproject.example.app/org.qtproject.qt.android.bindings.QtActivity  # Qt6
