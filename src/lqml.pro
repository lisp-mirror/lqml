QT          += quick qml
TEMPLATE    = app
CONFIG      += no_keywords release
INCLUDEPATH = /usr/local/include
LIBS        = -L/usr/local/lib -lecl -llisp
TARGET      = lqml
DESTDIR     = .
OBJECTS_DIR = ./tmp
MOC_DIR     = ./tmp

# optional (uncomment if wanted)
# for running example 'clog-demo' on the desktop
# requires Qt WebEngine to be installed
#QT          += webview
#DEFINES     += INI_WEBVIEW

linux {
  LIBS        += -L../../platforms/linux/lib
  target.path = /usr/bin
}

macx {
  CONFIG      -= app_bundle
  LIBS        += -L../../platforms/macos/lib
  target.path = /usr/local/bin
}

INSTALLS = target

HEADERS += \
  cpp/marshal.h \
  cpp/ecl_ext.h \
  cpp/qml_ext.h \
  cpp/lqml.h \
  cpp/qt_ecl.h \
  cpp/single_shot.h \
  cpp/main.h

SOURCES += \
  cpp/marshal.cpp \
  cpp/ecl_ext.cpp \
  cpp/qml_ext.cpp \
  cpp/lqml.cpp \
  cpp/qt_ecl.cpp \
  cpp/single_shot.cpp \
  cpp/main.cpp

QMAKE_CXXFLAGS += -std=c++17

QMAKE_PRE_LINK = ecl -shell $$PWD/make.lisp

