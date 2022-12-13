# This creates a signed APK for the PlayStore.
#
# (1) run 'make apk' before running this script
#
# (2) don't set min sdk and target sdk in 'AndroidManifest.xml'; instead,
#     set them in 'android-build/build.gradle', see section 'defaultConfig'.

~/Qt/5.15.2/android/bin/androiddeployqt \
  --input android-app-deployment-settings.json \
  --output android-build \
  --release \
  --sign <path-to-keystore-file>/my.keystore repl
