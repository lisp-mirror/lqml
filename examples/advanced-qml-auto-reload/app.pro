LISP_FILES = $$files(lisp/*) app.asd make.lisp

android {
  lisp.commands = $$(ECL_ANDROID)/../ecl-android-host/bin/ecl \
                  -norc -shell $$PWD/make.lisp
} else:ios {
  lisp.commands = $$(ECL_IOS)/../ecl-ios-host/bin/ecl \
                  -norc -shell $$PWD/make.lisp
} else:unix {
  lisp.commands = /usr/local/bin/ecl -shell $$PWD/make.lisp
}

lisp.input  = LISP_FILES
lisp.output = tmp/libapp.a

QMAKE_EXTRA_COMPILERS += lisp
PRE_TARGETDEPS        += tmp/libapp.a

QT          += quick qml
TEMPLATE    = app
CONFIG      += no_keywords release
DEFINES     += INI_LISP INI_ECL_CONTRIB
INCLUDEPATH = /usr/local/include
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

android {
  QT          += androidextras
  INCLUDEPATH = $$(ECL_ANDROID)/include
  ECL_VERSION = $$lower($$system($ECL_ANDROID/../ecl-android-host/bin/ecl -v))
  ECL_VERSION = $$replace(ECL_VERSION, " ", "-")
  LIBS        = -L$$(ECL_ANDROID)/lib -lecl
  LIBS        += -L$$(ECL_ANDROID)/lib/$$ECL_VERSION
  LIBS        += -lecl-help -ldeflate -lecl-cdb -lecl-curl -lql-minitar -lsockets
  LIBS        += -L../../../platforms/android/lib

  ANDROID_ABIS               = "arm64-v8a"
  ANDROID_EXTRA_LIBS         += $$(ECL_ANDROID)/lib/libecl.so
  ANDROID_PACKAGE_SOURCE_DIR = ../platforms/android
}

ios {
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

LIBS    += -llqml -llisp -Ltmp -lapp
HEADERS += ../../src/cpp/main.h
SOURCES += ../../src/cpp/main.cpp

system(ecl -shell qml/.create-qml-loaders.lisp)

RESOURCES += $$files(qml/*)
RESOURCES += $$files(qml/.ext/*)

QMAKE_CXXFLAGS += -std=c++17

