CONFIG += 32bit

LISP_FILES = $$files(lisp/*) app.asd make.lisp

android {
  32bit {
    ECL = $$(ECL_ANDROID_32)
  } else {
    ECL = $$(ECL_ANDROID)
  }
  lisp.commands = $$ECL/../ecl-android-host/bin/ecl \
                  -norc -shell $$PWD/make.lisp
} else:unix {
  lisp.commands = /usr/local/bin/ecl -shell $$PWD/make.lisp
} else:win32 {
  lisp.commands = ecl.exe -shell $$PWD/make.lisp
}

lisp.input = LISP_FILES

win32:  lisp.output = tmp/app.lib
!win32: lisp.output = tmp/libapp.a

QMAKE_EXTRA_COMPILERS += lisp

win32:  PRE_TARGETDEPS = tmp/app.lib
!win32: PRE_TARGETDEPS = tmp/libapp.a

QT          += quick qml sensors positioning
TEMPLATE    = app
CONFIG      += c++17 no_keywords release
DEFINES     = DESKTOP_APP INI_LISP #INI_ECL_CONTRIB
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
  QT          += androidextras
  #DEFINES     += INI_ASDF
  DEFINES     -= DESKTOP_APP
  DEFINES     += QT_EXTENSION
  INCLUDEPATH = $$ECL/include
  LIBS        = -L$$ECL/lib -lecl
  LIBS        += -L../../../platforms/android/lib
  HEADERS     += cpp/qt.h
  SOURCES     += cpp/qt.cpp

  ANDROID_EXTRA_LIBS         += $$ECL/lib/libecl.so
  ANDROID_PACKAGE_SOURCE_DIR = ../platforms/android

  32bit {
    ANDROID_ABIS = "armeabi-v7a"
  } else {
    ANDROID_ABIS = "arm64-v8a"
  }
}

32bit {
  LIBS += -llqml32 -llisp32
} else {
  LIBS += -llqml -llisp
}

LIBS    += -Ltmp -lapp
HEADERS += ../../src/cpp/main.h
SOURCES += ../../src/cpp/main.cpp

RESOURCES += $$files(qml/*)
RESOURCES += $$files(i18n/*.qm)

lupdate_only {
  SOURCES += i18n/tr.h
}
