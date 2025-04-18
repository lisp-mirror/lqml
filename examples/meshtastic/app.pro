# to set manually (when no BLE available)
#CONFIG += no_ble

LISP_FILES = $$files(lisp/*) app.asd make.lisp

exists(/etc/sailfish-release) {
  CONFIG += sfos
}

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
  RC_ICONS = platforms/windows/icon.ico

  include(../../src/windows.pri)
}

sfos {
  QT      -= serialport
  CONFIG  += no_usb
}

android {
  QT          += remoteobjects
  QT          -= serialport
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
  ANDROID_TARGET_SDK_VERSION = 34
  ANDROID_EXTRA_LIBS         += $$ECL/lib/libecl.so
  ANDROID_PACKAGE_SOURCE_DIR = ../platforms/android

  # OpenSSL libs can be downloaded from: https://github.com/KDAB/android_openssl
  # required for downloading map tiles (please note naming convention for Qt)
  # N.B. if you need SSL on Lisp side also, remove '_1_1' from the name
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
  QT          -= serialport
  CONFIG      += no_usb
  DEFINES     += INI_ASDF NO_USB
  DEFINES     -= DESKTOP_APP
  INCLUDEPATH = $$(ECL_IOS)/include
  ECL_VERSION = $$lower($$system($ECL_IOS/../ecl-ios-host/bin/ecl -v))
  ECL_VERSION = $$replace(ECL_VERSION, " ", "-")
  LIBS        = -L$$(ECL_IOS)/lib -lecl
  LIBS        += -leclatomic -leclffi -leclgc -leclgmp
  LIBS        += -L$$(ECL_IOS)/lib/$$ECL_VERSION
  LIBS        += -lasdf -lecl-help -ldeflate -lecl-cdb -lecl-curl -lql-minitar -lsockets
  LIBS        += -L../../../platforms/ios/lib

  # note: SSL on Qt side works out of the box on iOS

  SOURCES += cpp/ios.mm

  QMAKE_INFO_PLIST     = platforms/ios/Info.plist
  QMAKE_ASSET_CATALOGS += platforms/ios/Assets.xcassets

  assets.files      = $$files($$PWD/platforms/ios/assets)
  QMAKE_BUNDLE_DATA += assets
  launch.files      = platforms/ios/designable.storyboard platforms/img/logo.png
  QMAKE_BUNDLE_DATA += launch
}

no_ble {
  DEFINES += NO_BLE
  QT -= bluetooth
}
no_usb {
  DEFINES += NO_USB
}

32bit {
  android {
    equals(QT_MAJOR_VERSION, 6) {
      LIBS += -llqml32_armeabi-v7a
    }
    lessThan(QT_MAJOR_VERSION, 6) {
      LIBS += -llqml32
    }
  }
  !android {
    LIBS += -llqml32
  }
  LIBS += -llisp32
} else {
  android {
    equals(QT_MAJOR_VERSION, 6) {
      LIBS += -llqml_arm64-v8a
    }
    lessThan(QT_MAJOR_VERSION, 6) {
      LIBS += -llqml
    }
  }
  !android {
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
    cpp/connection/wifi/wifi_me.h

  SOURCES += \
    cpp/connection/connection.cpp \
    cpp/connection/wifi/wifi_me.cpp

  !no_ble {
    HEADERS += \
      cpp/connection/ble/ble.h \
      cpp/connection/ble/ble_me.h
    SOURCES += \
      cpp/connection/ble/ble.cpp \
      cpp/connection/ble/ble_me.cpp
  }

  !no_usb {
    HEADERS += \
      cpp/connection/usb/usb_me.h
    SOURCES += \
      cpp/connection/usb/usb_me.cpp
  }
}

RESOURCES += $$files(qml/*)
RESOURCES += $$files(i18n/*.qm)
RESOURCES += $$files(lisp/proto/meshtastic/*.lisp)

lupdate_only {
  SOURCES += i18n/tr.h
}
