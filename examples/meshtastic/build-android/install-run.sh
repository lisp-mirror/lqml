# install/update (keeps app data)
adb install -r android-build/*.apk
adb shell am start -n org.cl.meshtastic/.MeActivity
