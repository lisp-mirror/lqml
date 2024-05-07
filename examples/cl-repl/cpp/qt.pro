QT          += gui quick network
TEMPLATE    = lib
CONFIG      += c++17 plugin release no_keywords
DEFINES     += PLUGIN
INCLUDEPATH = /usr/local/include ../../../src/cpp
LIBS        = -L/usr/local/lib -lecl
DESTDIR     = ./
TARGET      = qt
OBJECTS_DIR = ./tmp/
MOC_DIR     = ./tmp/

HEADERS += qt.h
SOURCES += qt.cpp

linux {
  QT      += serialport
  HEADERS += usb/usb.h
  SOURCES += usb/usb.cpp
  LIBS    += -L../../../platforms/linux/lib
}

macx {
  LIBS += -L../../../platforms/macos/lib
}

win32 {
  include(../../../src/windows.pri)

  LIBS += -L../../../platforms/windows/lib
}
