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
DEFINES     = DESKTOP_APP INI_LISP
INCLUDEPATH = /usr/local/include
LIBS        = -L/usr/local/lib -lecl
DESTDIR     = .
TARGET      = app
OBJECTS_DIR = tmp
MOC_DIR     = tmp

linux: LIBS += -L../../../platforms/linux/lib
macx:  LIBS += -L../../../platforms/macos/lib
win32: LIBS += -L../../../platforms/windows/lib

win32 {
  include(../../src/windows.pri)
}

android {
  QT          += androidextras sensors
  DEFINES     -= DESKTOP_APP
  INCLUDEPATH = $$(ECL_ANDROID)/include
  LIBS        = -L$$(ECL_ANDROID)/lib -lecl
  LIBS        += -L../../../platforms/android/lib

  ANDROID_ABIS               = "arm64-v8a"
  ANDROID_EXTRA_LIBS         += $$(ECL_ANDROID)/lib/libecl.so
  ANDROID_PACKAGE_SOURCE_DIR = ../platforms/android
}

ios {
  QT          += sensors
  DEFINES     -= DESKTOP_APP
  INCLUDEPATH = $$(ECL_IOS)/include
  LIBS        = -L$$(ECL_IOS)/lib -lecl
  LIBS        += -leclatomic -leclffi -leclgc -leclgmp
  LIBS        += -L../../../platforms/ios/lib

  QMAKE_INFO_PLIST = platforms/ios/Info.plist
}

LIBS    += -llqml -llisp -Ltmp -lapp
HEADERS += ../../src/cpp/main.h
SOURCES += ../../src/cpp/main.cpp

RESOURCES = $$files(qml/*)

QMAKE_CXXFLAGS += -std=c++17
