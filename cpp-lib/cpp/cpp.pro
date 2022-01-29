QT          += widgets
TEMPLATE    = lib
CONFIG      += plugin release no_keywords
LIBS        += -L/usr/local/lib -lecl
DESTDIR     = ../
TARGET      = cpp
OBJECTS_DIR = ./tmp/
MOC_DIR     = ./tmp/

win32 {
  include(../../src/windows.pri)
}

# 'marshal.*' and 'qt_ecl.*' only needed for calling Lisp

HEADERS += \
  lib.h \
  ../../src/cpp/marshal.h \
  ../../src/cpp/qt_ech.h

SOURCES += \
  lib.cpp \
  ../../src/cpp/marshal.cpp \
  ../../src/cpp/qt_ecl.cpp
