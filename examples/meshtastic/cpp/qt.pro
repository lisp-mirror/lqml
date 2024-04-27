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

HEADERS += \
  connection/connection.h \
  connection/ble/ble.h \
  connection/ble/ble_me.h \
  connection/usb/usb_me.h \
  qt.h

SOURCES += \
  connection/connection.cpp \
  connection/ble/ble.cpp \
  connection/ble/ble_me.cpp \
  connection/usb/usb_me.cpp \
  qt.cpp

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
