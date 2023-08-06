# This creates a signed APK for the PlayStore.
#
# (1) run 'make apk' before running this script
#
# (2) don't set min sdk and target sdk in 'AndroidManifest.xml'; instead,
#     set them in 'app.pro'

# hack to not include Qt Widgets on mobile (not used)
rm -f android-build/libs/arm64-v8a/libQt5Widgets_arm64-v8a.so
mv ../qml/ext/dialogs/Confirm.qml ../qml/ext/dialogs/.Confirm.qml
mv ../qml/ext/dialogs/Message.qml ../qml/ext/dialogs/.Message.qml

~/Qt/5.15.2/android/bin/androiddeployqt \
  --input android-app-deployment-settings.json \
  --output android-build \
  --release \
  --sign <path-to-keystore-file>/my.keystore repl

# restore
mv ../qml/ext/dialogs/.Confirm.qml ../qml/ext/dialogs/Confirm.qml
mv ../qml/ext/dialogs/.Message.qml ../qml/ext/dialogs/Message.qml
