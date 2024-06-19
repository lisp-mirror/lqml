# to set manually (when no BLE available)
#CONFIG += no_ble

QT          += sql network bluetooth serialport
TEMPLATE    = lib
CONFIG      += c++17 plugin release no_keywords
DEFINES     += PLUGIN
INCLUDEPATH = /usr/local/include ../../../src/cpp
LIBS        = -L/usr/local/lib -lecl
DESTDIR     = ./
TARGET      = qt
OBJECTS_DIR = ./tmp/
MOC_DIR     = ./tmp/

no_ble {
  DEFINES += NO_BLE
  QT -= bluetooth
}

HEADERS += \
  connection/connection.h \
  connection/usb/usb_me.h \
  connection/wifi/wifi_me.h \
  qt.h

SOURCES += \
  connection/connection.cpp \
  connection/usb/usb_me.cpp \
  connection/wifi/wifi_me.cpp \
  qt.cpp

!no_ble {
  HEADERS += \
    connection/ble/ble.h \
    connection/ble/ble_me.h
  SOURCES += \
    connection/ble/ble.cpp \
    connection/ble/ble_me.cpp
}

linux {
  LIBS += -L../../../platforms/linux/lib
}

macx {
  LIBS += -L../../../platforms/macos/lib
}

win32 {
  include(../../../src/windows.pri)

  LIBS += -L../../../platforms/windows/lib
}
