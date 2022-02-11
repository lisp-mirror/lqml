# build the host ECL, which will then be used
# to build the cross-compiled Android version
# (assumes a 64bit platform)

./configure CFLAGS="-g -O2" LDFLAGS="-g -O2" CC=clang \
            --prefix=`pwd`/ecl-android-host \
            --disable-c99complex \
            --enable-manual=no
make
make install
rm -r build
