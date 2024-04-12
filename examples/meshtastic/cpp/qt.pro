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
  ble/ble.h \
  ble/ble_meshtastic.h \
  usb/usb_meshtastic.h \
  qt.h

SOURCES += \
  ble/ble.cpp \
  ble/ble_meshtastic.cpp \
  usb/usb_meshtastic.cpp \
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
