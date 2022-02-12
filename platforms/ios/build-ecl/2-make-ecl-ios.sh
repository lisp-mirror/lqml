#!/bin/sh

script_dir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
source_dir=$script_dir

config_options="--disable-shared --disable-c99complex --enable-manual=no --with-cross-config=$source_dir/src/util/iOS-arm64.cross_config"

# please note: we need to add -DGC_DISABLE_INCREMENTAL, which will remove a private API call
# from the binary, since those are not allowed in iOS (for security reasons)

export CFLAGS="-DECL_C_COMPATIBLE_VARIADIC_DISPATCH -DECL_RWLOCK -DGC_DISABLE_INCREMENTAL"

build_dir="$source_dir/build_ios/"
install_prefix="$source_dir/ecl-ios/"

mkdir -p "$build_dir"
mkdir -p "$install_prefix"

num_simultaneous_jobs=1

select_ios()
{
  platform_type="$1"
  arch="$2"

  ios_platform="$platform_type.platform"

  developer_dir="`xcode-select --print-path`"
  platforms_dir="$developer_dir/Platforms"
  ios_platform_dir="$platforms_dir/$ios_platform"
  ios_sdks="$ios_platform_dir/Developer/SDKs"

  sdk_version=`(cd "$ios_sdks"; ls -1d *.sdk |sed -e 's/\.sdk$//' -e 's/^[^0-9\.]*//' |awk 'BEGIN{best = 0.0}($0 + 0.0) > best + 0.0{best = $0;}END{print best}')`
  ios_sdk="$platform_type$sdk_version.sdk"

  ios_sdk_dir="$ios_sdks/$ios_sdk"

  echo "*** Selecting platform \"$ios_platform\" and SDK \"$ios_sdk\" for \"$arch\"."

  case "$platform_type" in

           iPhoneOS) config_options_extras=--host=aarch64-apple-darwin
                     sdk_name="iphoneos"
                     ;;

    iPhoneSimulator) config_options_extras=
                     sdk_name="iphonesimulator"
                     ;;

  esac

  # this needs to be updated when a new Xcode is released
  iphoneos_version_min="11.0"

  export CC="clang"
  export CXX="clang++"

  export CFLAGS="$CFLAGS -arch $arch -miphoneos-version-min=$iphoneos_version_min -isysroot $ios_sdk_dir"
  export CFLAGS="$CFLAGS -pipe -Wno-trigraphs -Wreturn-type -Wunused-variable"
  export CFLAGS="$CFLAGS -fpascal-strings -fasm-blocks -fmessage-length=0 -fvisibility=hidden"
  export CFLAGS="$CFLAGS -O0 -DNO_ASM"

  export CXXFLAGS="$CFLAGS"

  # export LD="ld -arch $arch"
  export LD="ld"
  export LDFLAGS="-arch $arch -pipe -std=c99 -gdwarf-2 -isysroot $ios_sdk_dir"

  export LIBS="-framework Foundation"
}

configure_ecl()
{
  cd "$build_dir"
  $source_dir/src/configure --prefix="$install_prefix" $config_options $config_options_extras
  cd -
}

make_ecl()
{
  cd "$build_dir"

  # make clean

  make -j $num_simultaneous_jobs || exit 1

  make install

  cd -
}

build_one_ios()
{
  platform_type="$1"
  arch="$2"

  select_ios "$platform_type" "$arch"

  configure_ecl

  make_ecl
}

export ECL_TO_RUN="../ecl-ios-host/bin/ecl"

build_one_ios iPhoneOS arm64
