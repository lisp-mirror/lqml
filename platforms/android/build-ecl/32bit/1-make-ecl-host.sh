# build the host ECL, which will then be used to build the cross-compiled
# android version (32bit)

./configure ABI=32 CFLAGS="-m32 -g -O2" LDFLAGS="-m32 -g -O2" CC=clang \
            --prefix=`pwd`/ecl-android-host \
            --disable-c99complex \
            --enable-manual=no
make
make install
rm -r build
