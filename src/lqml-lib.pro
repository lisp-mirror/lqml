QT          += quick qml
TEMPLATE    = lib
CONFIG      += c++17 staticlib no_keywords release
LIBS        = -L/usr/local/lib -lecl
TARGET      = lqml
OBJECTS_DIR = ./tmp
MOC_DIR     = ./tmp

linux {
  INCLUDEPATH = /usr/local/include
  DESTDIR     = ../../platforms/linux/lib
}

macx {
  INCLUDEPATH = /usr/local/include
  DESTDIR     = ../../platforms/macos/lib
}

win32 {
  include(windows.pri)

  DESTDIR = ../../platforms/windows/lib
}

android {
  QT           += androidextras
  INCLUDEPATH  = $$(ECL_ANDROID)/include
  LIBS         = -L$$(ECL_ANDROID)/lib -lecl
  DESTDIR      = ../../platforms/android/lib
  ANDROID_ABIS = "arm64-v8a"
}

ios {
  INCLUDEPATH = $$(ECL_IOS)/include
  LIBS        = -L$$(ECL_IOS)/lib -lecl
  DESTDIR     = ../../platforms/ios/lib
}

HEADERS += \
  cpp/marshal.h \
  cpp/ecl_ext.h \
  cpp/qml_ext.h \
  cpp/lqml.h \
  cpp/qt_ecl.h \
  cpp/single_shot.h

SOURCES += \
  cpp/marshal.cpp \
  cpp/ecl_ext.cpp \
  cpp/qml_ext.cpp \
  cpp/lqml.cpp \
  cpp/qt_ecl.cpp \
  cpp/single_shot.cpp

# compile Lisp code

android {
  QMAKE_POST_LINK = $$(ECL_ANDROID)/../ecl-android-host/bin/ecl \
                    -norc -shell $$PWD/make.lisp
} else:ios {
  QMAKE_POST_LINK = ../../platforms/ios/cross-compile.sh ../make.lisp
} else {
  QMAKE_POST_LINK = ecl -shell $$PWD/make.lisp
}

