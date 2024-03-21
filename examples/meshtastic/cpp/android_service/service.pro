QT += core bluetooth remoteobjects
TEMPLATE = lib
CONFIG += c++17 dll
INCLUDEPATH += $$PWD
TARGET = service
DESTDIR = ../../build-android
OBJECTS_DIR = ./tmp
MOC_DIR     = ./tmp

equals(QT_MAJOR_VERSION, 6) {
  QT += core-private
}
lessThan(QT_MAJOR_VERSION, 6) {
  QT += androidextras
}

HEADERS += \
  ../ble/ble.h \
  ../ble/ble_meshtastic.h \
  qtandroidservice_ro.h

SOURCES += \
  ../ble/ble.cpp \
  ../ble/ble_meshtastic.cpp \
  main.cpp

REPC_SOURCE += qtandroidservice.rep

32bit {
  ANDROID_ABIS = "armeabi-v7a"
} else {
  ANDROID_ABIS = "arm64-v8a"
}
