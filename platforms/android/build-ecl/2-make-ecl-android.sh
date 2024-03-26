# use the previously built host ECL to build the android version
# tested with NDK 21
# you need to define ANDROID_NDK_TOOLCHAIN

export AR=$ANDROID_NDK_TOOLCHAIN/bin/aarch64-linux-android-ar
export AS=$ANDROID_NDK_TOOLCHAIN/bin/aarch64-linux-android-as
export CC=$ANDROID_NDK_TOOLCHAIN/bin/aarch64-linux-android21-clang
export LD=$ANDROID_NDK_TOOLCHAIN/bin/aarch64-linux-android-ld
export RANLIB=$ANDROID_NDK_TOOLCHAIN/bin/aarch64-linux-android-ranlib
export STRIP=$ANDROID_NDK_TOOLCHAIN/bin/aarch64-linux-android-strip

export ECL_TO_RUN=`pwd`/ecl-android-host/bin/ecl

./configure --host=aarch64-linux-android \
            --prefix=`pwd`/ecl-android \
            --disable-c99complex \
            --enable-manual=no \
            --with-cross-config=`pwd`/src/util/android-arm64.cross_config
make
make install
