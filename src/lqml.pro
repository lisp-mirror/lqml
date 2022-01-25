LISP_FILES = \
  make.lisp \
  lisp/x.lisp \
  lisp/package.lisp \
  lisp/ini.lisp \
  lisp/qml.lisp \
  lqml.asd

lisp.output   = liblqml.a
lisp.commands = ecl -shell $$PWD/make.lisp
lisp.input    = LISP_FILES

QMAKE_EXTRA_COMPILERS += lisp

QT          += quick qml
TEMPLATE    = app
CONFIG      += no_keywords release
INCLUDEPATH += /usr/local/include
LIBS        += -L/usr/local/lib -lecl -L. -llqml
TARGET      = lqml
DESTDIR     = .

linux {
  target.path = /usr/bin
}

osx {
  CONFIG -= app_bundle
  target.path = /usr/local/bin
}

INSTALLS = target

win32 {
  include(windows.pri)
}

HEADERS += \
  cpp/marshal.h \
  cpp/ecl_ext.h \
  cpp/lqml.h \
  cpp/qml.h \
  cpp/qt_ecl.h \
  cpp/single_shot.h

SOURCES += \
  cpp/marshal.cpp \
  cpp/ecl_ext.cpp \
  cpp/lqml.cpp \
  cpp/qml.cpp \
  cpp/qt_ecl.cpp \
  cpp/main.cpp

QMAKE_CXXFLAGS += -std=c++17

