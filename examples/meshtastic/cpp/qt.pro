QT          += bluetooth sql positioning network
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
  ble.h \
  ble_meshtastic.h \
  tile_server.h \
  qt.h

SOURCES += \
  ble.cpp \
  ble_meshtastic.cpp \
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
