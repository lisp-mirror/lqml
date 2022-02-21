QT          += widgets
TEMPLATE    = lib
CONFIG      += plugin release no_keywords
INCLUDEPATH = /usr/local/include
LIBS        = -L/usr/local/lib -lecl
DESTDIR     = ../
TARGET      = cpp
OBJECTS_DIR = ./tmp/
MOC_DIR     = ./tmp/

HEADERS += lib.h
SOURCES += lib.cpp
