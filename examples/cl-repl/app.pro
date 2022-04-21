LISP_FILES = $$files(lisp/*) app.asd make.lisp

android {
  lisp.commands = $$(ECL_ANDROID)/../ecl-android-host/bin/ecl \
                  -norc -shell $$PWD/make.lisp
} else:ios {
  lisp.commands = $$(ECL_IOS)/../ecl-ios-host/bin/ecl \
                  -norc -shell $$PWD/make.lisp
} else:unix {
  lisp.commands = /usr/local/bin/ecl -shell $$PWD/make.lisp
} else:win32 {
  lisp.commands = ecl.exe -shell $$PWD/make.lisp
}

lisp.input  = LISP_FILES

win32:  lisp.output = tmp/app.lib
!win32: lisp.output = tmp/libapp.a

QMAKE_EXTRA_COMPILERS += lisp

win32:  PRE_TARGETDEPS = tmp/app.lib
!win32: PRE_TARGETDEPS = tmp/libapp.a

QT          += quick qml
TEMPLATE    = app
CONFIG      += no_keywords release
DEFINES     += DESKTOP_APP INI_LISP INI_ECL_CONTRIB QT_EXTENSION
INCLUDEPATH = /usr/local/include ../../../src/cpp
ECL_VERSION = $$lower($$system(ecl -v))
ECL_VERSION = $$replace(ECL_VERSION, " ", "-")
LIBS        = -L/usr/local/lib -lecl
LIBS        += -L/usr/local/lib/$$ECL_VERSION
LIBS        += -lecl-help -ldeflate -lecl-cdb -lecl-curl -lql-minitar -lsockets
DESTDIR     = .
TARGET      = app
OBJECTS_DIR = tmp
MOC_DIR     = tmp

linux: LIBS += -L../../../platforms/linux/lib
macx:  LIBS += -L../../../platforms/macos/lib
win32: LIBS += -L../../../platforms/windows/lib

win32 {
  LIBS += ws2_32

  include(../../src/windows.pri)
}

android {
  QT          += androidextras
  DEFINES     += INI_ASDF NO_TEXT_HANDLES
  DEFINES     -= DESKTOP_APP
  INCLUDEPATH = $$(ECL_ANDROID)/include
  ECL_VERSION = $$lower($$system($ECL_ANDROID/../ecl-android-host/bin/ecl -v))
  ECL_VERSION = $$replace(ECL_VERSION, " ", "-")
  LIBS        = -L$$(ECL_ANDROID)/lib -lecl
  LIBS        += -L$$(ECL_ANDROID)/lib/$$ECL_VERSION
  LIBS        += -lasdf -lecl-help -ldeflate -lecl-cdb -lecl-curl -lql-minitar -lsockets
  LIBS        += -L../../../platforms/android/lib

  ANDROID_ABIS               = "arm64-v8a"
  ANDROID_EXTRA_LIBS         += $$(ECL_ANDROID)/lib/libecl.so
  ANDROID_PACKAGE_SOURCE_DIR = ../platforms/android
}

ios {
  DEFINES     += INI_ASDF NO_TEXT_HANDLES
  DEFINES     -= DESKTOP_APP
  INCLUDEPATH = $$(ECL_IOS)/include
  ECL_VERSION = $$lower($$system($ECL_IOS/../ecl-ios-host/bin/ecl -v))
  ECL_VERSION = $$replace(ECL_VERSION, " ", "-")
  LIBS        = -L$$(ECL_IOS)/lib -lecl
  LIBS        += -leclatomic -leclffi -leclgc -leclgmp
  LIBS        += -L$$(ECL_IOS)/lib/$$ECL_VERSION
  LIBS        += -lasdf -lecl-help -ldeflate -lecl-cdb -lecl-curl -lql-minitar -lsockets
  LIBS        += -L../../../platforms/ios/lib

  assets.files      = $$files($$PWD/platforms/ios/assets)
  QMAKE_BUNDLE_DATA += assets
}

INCLUDEPATH += ../../../src/cpp
LIBS        += -llqml -llisp -Ltmp -lapp

HEADERS += ../../src/cpp/main.h   cpp/qt.h
SOURCES += ../../src/cpp/main.cpp cpp/qt.cpp

RESOURCES = $$files(qml/*)

QMAKE_CXXFLAGS += -std=c++17

