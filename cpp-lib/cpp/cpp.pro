QT          += widgets
TEMPLATE    = lib
CONFIG      += plugin release
DESTDIR     = ../
TARGET      = cpp
OBJECTS_DIR = ./tmp/
MOC_DIR     = ./tmp/

win32 {
  include(../../src/windows.pri)
}

HEADERS += lib.h
SOURCES += lib.cpp
