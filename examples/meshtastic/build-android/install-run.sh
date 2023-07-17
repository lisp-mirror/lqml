# install/update (keeps app data)
adb install -r android-build/*.apk
adb shell am start -n org.cl.meshtastic/org.qtproject.qt5.android.bindings.QtActivity # Qt5
