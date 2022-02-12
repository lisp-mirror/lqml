# set up all environment variables needed for cross-compiling
#
# they will be read into ECL using (ext:getenv) in order to
# set the C compiler variables

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

  export CFLAGS="-arch $arch -miphoneos-version-min=$iphoneos_version_min -isysroot $ios_sdk_dir"
  export CFLAGS="$CFLAGS -pipe -Wno-trigraphs -Wreturn-type -Wunused-variable"
  export CFLAGS="$CFLAGS -fpascal-strings -fasm-blocks -fmessage-length=0 -fvisibility=hidden"
  export CFLAGS="$CFLAGS -O0 -DNO_ASM"

  export CXXFLAGS="$CFLAGS"

  export LD="ld"
  export LDFLAGS="-arch $arch -miphoneos-version-min=$iphoneos_version_min -pipe -std=c99 -gdwarf-2 -isysroot $ios_sdk_dir"

  export LIBS="-framework Foundation"
}

select_ios "iPhoneOS" "arm64"
#select_ios "iPhoneSimulator" "x86_64"

$ECL_IOS/../ecl-ios-host/bin/ecl -norc -shell $1

