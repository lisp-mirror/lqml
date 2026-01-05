# Dockerfile
#
# - `docker build -f Dockerfile -t sokoban-android .`
# - `docker run -it sokoban-android`
# - follow instructions that are printed
#
# This file downloads required files to make it easier to share. If you're
# going to run this repeatedly (in CI for example) then download the files
# only once outside of this build and copy them inside during the build.
# Otherwise you'll be wasting a lot of someone else's bandwidth.
#
# The files:
# - `1-make-ecl-host.sh`
# - `2-make-ecl-android.sh`
# - `ecl-23.9.9.tgz`
# - `qt-everywhere-opensource-src-5.15.15.tar.xz`

FROM mingc/android-build-box:1.23.1

# There might be some superfluous definitions here :shrug:
# These are copied from running the Android Build Box.
ENV ANDROID_HOME="/opt/android-sdk"
ENV ANDROID_NDK_HOME="/opt/android-sdk/ndk/current"
ENV ANDROID_NDK="/opt/android-sdk/ndk/current"
ENV JAVA_HOME="/usr/lib/jvm/java-11-openjdk-amd64"
# These are required by LQML.
ENV ANDROID_NDK_ROOT="/opt/android-sdk/ndk/current"
ENV ANDROID_NDK_TOOLCHAIN="/opt/android-sdk/ndk/current/toolchains/llvm/prebuilt/linux-x86_64"
ENV ECL_ANDROID="/root/ecl/android/ecl-android"

# `--allow-insecure-repositories` because old repo, key expired
RUN apt-get update --allow-insecure-repositories
RUN apt-get install --yes clang
# This is probably due to using the Android Build Box.  We need this
# otherwise `(require :ecl-quicklisp)` gives "protocol not found: tcp".
RUN apt-get install --yes netbase

# We need NDK 21.3 and sdkmanager only works with Java 8.
RUN env JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64" sdkmanager --install "ndk;21.3.6528147"
WORKDIR /opt/android-sdk/ndk
RUN rm current
RUN ln -s /opt/android-sdk/ndk/21.3.6528147 current

# Create some directories we need later.
WORKDIR /root
RUN mkdir ecl qt

# Build ECL
WORKDIR /root/ecl
RUN curl --output ecl-23.9.9.tgz https://ecl.common-lisp.dev/static/files/release/ecl-23.9.9.tgz
RUN tar xvf ecl-23.9.9.tgz
RUN rm ecl-23.9.9.tgz
RUN mv ecl-23.9.9 android
WORKDIR /root/ecl/android
RUN curl --output 1-make-ecl-host.sh "https://gitlab.com/eql/lqml/-/raw/master/platforms/android/build-ecl/1-make-ecl-host.sh?ref_type=heads&inline=false"
RUN curl --output 2-make-ecl-android.sh "https://gitlab.com/eql/lqml/-/raw/master/platforms/android/build-ecl/2-make-ecl-android.sh?ref_type=heads&inline=false"
RUN chmod 750 1-make-ecl-host.sh 2-make-ecl-android.sh
RUN ./1-make-ecl-host.sh
RUN ./2-make-ecl-android.sh

# Build QT (this takes a few hours on my machine)
WORKDIR /root/qt
RUN curl --location --output qt-everywhere-opensource-src-5.15.17.tar.xz https://download.qt.io/archive/qt/5.15/5.15.17/single/qt-everywhere-opensource-src-5.15.17.tar.xz
RUN tar xvf qt-everywhere-opensource-src-5.15.17.tar.xz
RUN rm qt-everywhere-opensource-src-5.15.17.tar.xz
WORKDIR /root/qt/qt-everywhere-src-5.15.17
RUN ./configure -xplatform android-clang --disable-rpath -nomake tests -nomake examples -android-ndk ${ANDROID_NDK_HOME} -android-sdk ${ANDROID_HOME} -no-warnings-are-errors -opensource -confirm-license
# `-j 4` seems to make a small difference in building speed
RUN make -j 4
RUN make install

# Build LQML library.
WORKDIR /root
RUN git clone https://gitlab.com/eql/lqml.git lqml
WORKDIR /root/lqml/src/build-android
RUN /usr/local/Qt-5.15.17/bin/qmake ../lqml-lib.pro
RUN make

# Build Sokoban.
WORKDIR /root/lqml/examples
RUN env PATH="$PATH:/root/ecl/android/ecl-android-host/bin" ./copy.sh sokoban
WORKDIR /root/lqml/examples/sokoban/build-android
RUN /usr/local/Qt-5.15.17/bin/qmake ../app.pro
RUN make apk

# Pause and show instruction for copying APK.
SHELL ["/bin/bash", "-c"]
CMD echo -e "In another terminal execute:\n    - 'docker cp ${HOSTNAME}:/root/lqml/examples/sokoban/build-android/android-build/app.apk .'\nto copy the APK out of the container." && read -p "Then press <Enter> to terminate this container."
