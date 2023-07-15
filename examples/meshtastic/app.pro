LISP_FILES = $$files(lisp/*) app.asd make.lisp

android {
  32bit {
    ECL = $$(ECL_ANDROID_32)
  } else {
    ECL = $$(ECL_ANDROID)
  }
  lisp.commands = $$ECL/../ecl-android-host/bin/ecl \
                  -norc -shell $$PWD/make.lisp
} else:ios {
  lisp.commands = $$(ECL_IOS)/../ecl-ios-host/bin/ecl \
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

QT          += quick qml bluetooth sql positioning
TEMPLATE    = app
CONFIG      += c++17 no_keywords release
DEFINES     += DESKTOP_APP INI_ECL_CONTRIB QT_EXTENSION BACKGROUND_INI_LISP
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
win32: LIBS += -L../../../platforms/windows/lib

win32 {
  LIBS += -lws2_32

  include(../../src/windows.pri)
}

android {
  QT          += androidextras
  DEFINES     -= DESKTOP_APP
  INCLUDEPATH = $$ECL/include
  ECL_VERSION = $$lower($$system($$ECL/../ecl-android-host/bin/ecl -v))
  ECL_VERSION = $$replace(ECL_VERSION, " ", "-")
  LIBS        = -L$$ECL/lib -lecl
  LIBS        += -L$$ECL/lib/$$ECL_VERSION
  LIBS        += -lecl-help -ldeflate -lecl-cdb -lecl-curl -lql-minitar -lsockets
  LIBS        += -L../../../platforms/android/lib

  ANDROID_EXTRA_LIBS         += $$ECL/lib/libecl.so
  ANDROID_PACKAGE_SOURCE_DIR = ../platforms/android
  ANDROID_MIN_SDK_VERSION    = 21
  ANDROID_TARGET_SDK_VERSION = 31

  32bit {
    ANDROID_ABIS = "armeabi-v7a"
  } else {
    ANDROID_ABIS = "arm64-v8a"
  }
}

ios {
  DEFINES     -= DESKTOP_APP
  INCLUDEPATH = $$(ECL_IOS)/include
  ECL_VERSION = $$lower($$system($ECL_IOS/../ecl-ios-host/bin/ecl -v))
  ECL_VERSION = $$replace(ECL_VERSION, " ", "-")
  LIBS        = -L$$(ECL_IOS)/lib -lecl
  LIBS        += -leclatomic -leclffi -leclgc -leclgmp
  LIBS        += -L$$(ECL_IOS)/lib/$$ECL_VERSION
  LIBS        += -lecl-help -ldeflate -lecl-cdb -lecl-curl -lql-minitar -lsockets
  LIBS        += -L../../../platforms/ios/lib

  QMAKE_INFO_PLIST     = platforms/ios/Info.plist
  #QMAKE_ASSET_CATALOGS += platforms/ios/Assets.xcassets

  #launch.files      = platforms/ios/designable.storyboard platforms/img/logo.png
  #QMAKE_BUNDLE_DATA += launch
}

32bit {
  LIBS += -llqml32 -llisp32
} else {
  LIBS += -llqml -llisp
}

LIBS        += -Ltmp -lapp
INCLUDEPATH += ../../../src/cpp

HEADERS += \
  ../../src/cpp/main.h \
  cpp/ble.h \
  cpp/ble_meshtastic.h \
  cpp/qt.h

SOURCES += \
  ../../src/cpp/main.cpp \
  cpp/ble.cpp \
  cpp/ble_meshtastic.cpp \
  cpp/qt.cpp

RESOURCES += $$files(qml/*)
RESOURCES += $$files(i18n/*.qm)

lupdate_only {
  SOURCES += i18n/tr.h
}
