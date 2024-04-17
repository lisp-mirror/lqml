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

QT          += quick qml quickcontrols2 bluetooth serialport sql location
TEMPLATE    = app
CONFIG      += c++17 no_keywords release
DEFINES     += DESKTOP_APP BACKGROUND_INI_LISP INI_ECL_CONTRIB QT_EXTENSION
INCLUDEPATH = /usr/local/include
ECL_VERSION = $$lower($$system(ecl -v))
ECL_VERSION = $$replace(ECL_VERSION, " ", "-")
LIBS        = -L/usr/local/lib -lecl
LIBS        += -L/usr/local/lib/$$ECL_VERSION
LIBS        += -lasdf -lecl-help -ldeflate -lecl-cdb -lecl-curl -lql-minitar -lsockets
DESTDIR     = .
TARGET      = app
OBJECTS_DIR = tmp
MOC_DIR     = tmp

linux: LIBS += -L../../../platforms/linux/lib
macx:  LIBS += -L../../../platforms/macos/lib
win32: LIBS += -L../../../platforms/windows/lib

macx {
  QMAKE_INFO_PLIST = platforms/macos/Info.plist
  ICON             = platforms/macos/Icon.icns
}

win32 {
  LIBS += -lws2_32

  include(../../src/windows.pri)
}

android {
  QT          += remoteobjects
  DEFINES     += INI_ASDF
  DEFINES     -= DESKTOP_APP
  INCLUDEPATH = $$ECL/include
  ECL_VERSION = $$lower($$system($$ECL/../ecl-android-host/bin/ecl -v))
  ECL_VERSION = $$replace(ECL_VERSION, " ", "-")
  LIBS        = -L$$ECL/lib -lecl
  LIBS        += -L$$ECL/lib/$$ECL_VERSION
  LIBS        += -lasdf -lecl-help -ldeflate -lecl-cdb -lecl-curl -lql-minitar -lsockets
  LIBS        += -L../../../platforms/android/lib

  equals(QT_MAJOR_VERSION, 6) {
    QT += core-private
  }
  lessThan(QT_MAJOR_VERSION, 6) {
    QT += androidextras
  }

  SOURCES += cpp/android.cpp

  REPC_REPLICA += cpp/android_service/qtandroidservice.rep

  ANDROID_MIN_SDK_VERSION    = 21
  ANDROID_TARGET_SDK_VERSION = 33
  ANDROID_EXTRA_LIBS         += $$ECL/lib/libecl.so
  ANDROID_PACKAGE_SOURCE_DIR = ../platforms/android

  # OpenSSL libs not included here, but can be downloaded from:
  # https://github.com/KDAB/android_openssl/tree/master/latest
  # required for downloading map tiles, please note naming convention for Qt:
  32bit {
    SSL_PATH = $$PWD/../../platforms/android/lib32
  } else {
    SSL_PATH = $$PWD/../../platforms/android/lib
  }
  ANDROID_EXTRA_LIBS += $$SSL_PATH/libcrypto_1_1.so $$SSL_PATH/libssl_1_1.so

  32bit {
    ANDROID_ABIS       = "armeabi-v7a"
    ANDROID_EXTRA_LIBS += libservice_armeabi-v7a.so
  } else {
    ANDROID_ABIS       = "arm64-v8a"
    ANDROID_EXTRA_LIBS += libservice_arm64-v8a.so
  }
}

ios {
  DEFINES     += INI_ASDF
  DEFINES     -= DESKTOP_APP
  INCLUDEPATH = $$(ECL_IOS)/include
  ECL_VERSION = $$lower($$system($ECL_IOS/../ecl-ios-host/bin/ecl -v))
  ECL_VERSION = $$replace(ECL_VERSION, " ", "-")
  LIBS        = -L$$(ECL_IOS)/lib -lecl
  LIBS        += -leclatomic -leclffi -leclgc -leclgmp
  LIBS        += -L$$(ECL_IOS)/lib/$$ECL_VERSION
  LIBS        += -lasdf -lecl-help -ldeflate -lecl-cdb -lecl-curl -lql-minitar -lsockets
  LIBS        += -L../../../platforms/ios/lib

  # OpenSSL libs not included here, required for downloading map tiles
  # either build them by yourself or find a trusted source for downloading,
  # and put them here: '../../../platforms/ios/lib/'
  LIBS        += -lcrypto -lssl

  SOURCES += cpp/ios.mm

  QMAKE_INFO_PLIST     = platforms/ios/Info.plist
  QMAKE_ASSET_CATALOGS += platforms/ios/Assets.xcassets

  assets.files      = $$files($$PWD/platforms/ios/assets)
  QMAKE_BUNDLE_DATA += assets
  launch.files      = platforms/ios/designable.storyboard platforms/img/logo.png
  QMAKE_BUNDLE_DATA += launch
}

32bit {
  equals(QT_MAJOR_VERSION, 6) {
    LIBS += -llqml32_armeabi-v7a
  }
  lessThan(QT_MAJOR_VERSION, 6) {
    LIBS += -llqml32
  }
  LIBS += -llisp32
} else {
  equals(QT_MAJOR_VERSION, 6) {
    LIBS += -llqml_arm64-v8a
  }
  lessThan(QT_MAJOR_VERSION, 6) {
    LIBS += -llqml
  }
  LIBS += -llisp
}

LIBS        += -Ltmp -lapp
INCLUDEPATH += ../../../src/cpp

HEADERS += \
  ../../src/cpp/main.h \
  cpp/qt.h

SOURCES += \
  ../../src/cpp/main.cpp \
  cpp/qt.cpp

!android {
  HEADERS += \
    cpp/connection/connection.h \
    cpp/connection/ble/ble.h \
    cpp/connection/ble/ble_meshtastic.h \
    cpp/connection/usb/usb_meshtastic.h

  SOURCES += \
    cpp/connection/connection.cpp \
    cpp/convention/ble/ble.cpp \
    cpp/convention/ble/ble_meshtastic.cpp \
    cpp/convention/usb/usb_meshtastic.cpp
}

RESOURCES += $$files(qml/*)
RESOURCES += $$files(i18n/*.qm)
RESOURCES += $$files(lisp/proto/meshtastic/*.lisp)

lupdate_only {
  SOURCES += i18n/tr.h
}
