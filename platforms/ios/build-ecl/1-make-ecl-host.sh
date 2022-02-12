# build host ECL, needed for cross-compiling

./configure --prefix=`pwd`/ecl-ios-host \
            --disable-c99complex \
            --enable-manual=no

make
make install
