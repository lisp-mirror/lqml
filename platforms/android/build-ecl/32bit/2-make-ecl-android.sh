# use the previously built host ECL to build the android version
# tested with NDK 21
# you need to define ANDROID_NDK_TOOLCHAIN

export CC=$ANDROID_NDK_TOOLCHAIN/bin/armv7a-linux-androideabi21-clang
export AR=$ANDROID_NDK_TOOLCHAIN/bin/arm-linux-androideabi-ar
export AS=$ANDROID_NDK_TOOLCHAIN/bin/arm-linux-androideabi-as
export LD=$ANDROID_NDK_TOOLCHAIN/bin/arm-linux-androideabi-ld
export RANLIB=$ANDROID_NDK_TOOLCHAIN/bin/arm-linux-androideabi-ranlib
export STRIP=$ANDROID_NDK_TOOLCHAIN/bin/arm-linux-androideabi-strip

export LDFLAGS="-fuse-ld=bfd"

export ECL_TO_RUN=`pwd`/ecl-android-host/bin/ecl

./configure --host=arm-linux-androideabi \
            --prefix=`pwd`/ecl-android \
            --disable-c99complex \
            --enable-manual=no \
            --with-cross-config=`pwd`/src/util/android-arm.cross_config
make
make install
