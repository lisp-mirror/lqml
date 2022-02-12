# changes to these files will re-compile the Lisp library when running 'make'
LISP_FILES = \
  lisp/package.lisp \
  lisp/main.lisp \
  app.asd \
  make.lisp

android {
  lisp.commands = $$(ECL_ANDROID)/../ecl-android-host/bin/ecl \
                  -norc -shell $$PWD/make.lisp
} else:ios {
  lisp.commands = $$(ECL_IOS)/../ecl-ios-host/bin/ecl \
                  -norc -shell $$PWD/make.lisp
} else:unix {
  lisp.commands = ecl -shell $$PWD/make.lisp
}

lisp.input  = LISP_FILES
lisp.output = tmp/libapp.a

QMAKE_EXTRA_COMPILERS += lisp

QT          += quick qml
TEMPLATE    = app
CONFIG      += no_keywords release
DEFINES     += INI_LISP
INCLUDEPATH = /usr/local/include
LIBS        = -L/usr/local/lib -lecl
DESTDIR     = .
TARGET      = app
OBJECTS_DIR = ./tmp
MOC_DIR     = ./tmp

linux: LIBS += -L../../../platforms/linux/lib -llqml -llisp -Ltmp -lapp
macx:  LIBS += -L../../../platforms/macos/lib -llqml -llisp -Ltmp -lapp

android {
  QT          += androidextras
  INCLUDEPATH = $$(ECL_ANDROID)/include
  LIBS        = -L$$(ECL_ANDROID)/lib -lecl
  LIBS        += -L../../../platforms/android/lib -llqml -llisp -Ltmp -lapp

  ANDROID_ABIS               = "arm64-v8a"
  ANDROID_EXTRA_LIBS         += $$(ECL_ANDROID)/lib/libecl.so
  #ANDROID_PACKAGE_SOURCE_DIR = ../platforms/android/sources
}

ios {
  INCLUDEPATH = $$(ECL_IOS)/include
  LIBS        = -L$$(ECL_IOS)/lib -lecl
  LIBS        += -leclatomic -leclffi -leclgc -leclgmp
  LIBS        += -L../../../platforms/ios/lib -llqml -llisp -Ltmp -lapp
}

SOURCES += ../../src/cpp/main.cpp

RESOURCES = app.qrc

QMAKE_CXXFLAGS += -std=c++17

