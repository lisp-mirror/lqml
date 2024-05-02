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
  ../connection/connection.h \
  ../connection/ble/ble.h \
  ../connection/ble/ble_me.h \
  ../connection/wifi/wifi_me.h \
  qtandroidservice_ro.h

SOURCES += \
  ../connection/connection.cpp \
  ../connection/ble/ble.cpp \
  ../connection/ble/ble_me.cpp \
  ../connection/wifi/wifi_me.cpp \
  main.cpp

REPC_SOURCE += qtandroidservice.rep

32bit {
  ANDROID_ABIS = "armeabi-v7a"
} else {
  ANDROID_ABIS = "arm64-v8a"
}
