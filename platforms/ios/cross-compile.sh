# set up all environment variables needed for cross-compiling
#
# they will be read into ECL using (ext:getenv) in order to
# set the C compiler variables
#
# compiles for both iOS and simulator

select_ios_and_simulator()
{
  arch_sim="`uname -m`"

  platform_type="iPhoneOS"
  platform_type_sim="iPhoneSimulator"

  developer_dir="`xcode-select --print-path`"
  platforms_dir="$developer_dir/Platforms"

  ios_platform="$platform_type.platform"
  ios_platform_sim="$platform_type_sim.platform"
  ios_platform_dir="$platforms_dir/$ios_platform"
  ios_platform_dir_sim="$platforms_dir/$ios_platform_sim"
  ios_sdks="$ios_platform_dir/Developer/SDKs"
  ios_sdks_sim="$ios_platform_dir_sim/Developer/SDKs"

  sdk_version=`(cd "$ios_sdks"; ls -1d *.sdk |sed -e 's/\.sdk$//' -e 's/^[^0-9\.]*//' |awk 'BEGIN{best = 0.0}($0 + 0.0) > best + 0.0{best = $0;}END{print best}')`
  sdk_version_sim=`(cd "$ios_sdks_sim"; ls -1d *.sdk |sed -e 's/\.sdk$//' -e 's/^[^0-9\.]*//' |awk 'BEGIN{best = 0.0}($0 + 0.0) > best + 0.0{best = $0;}END{print best}')`

  ios_sdk="$platform_type$sdk_version.sdk"
  ios_sdk_sim="$platform_type_sim$sdk_version_sim.sdk"

  ios_sdk_dir="$ios_sdks/$ios_sdk"
  ios_sdk_dir_sim="$ios_sdks_sim/$ios_sdk_sim"

  echo "*** compiling for \"$ios_platform\" - \"$ios_sdk\" and \"$ios_platform_sim\" - \"$ios_sdk_sim\""

  # this needs to be updated when a new Xcode is released
  iphoneos_version_min="11.0"

  export CC="clang"
  export CXX="clang++"

  export CFLAGS="-arch arm64 -arch $arch_sim -miphoneos-version-min=$iphoneos_version_min -isysroot $ios_sdk_dir -isysroot $ios_sdk_dir_sim"
  export CFLAGS="$CFLAGS -pipe -Wno-trigraphs -Wreturn-type -Wunused-variable"
  export CFLAGS="$CFLAGS -fpascal-strings -fasm-blocks -fmessage-length=0 -fvisibility=hidden"
  export CFLAGS="$CFLAGS -O0 -DNO_ASM"

  export CXXFLAGS="$CFLAGS"

  export LD="ld"
  export LDFLAGS="-arch arm64 -arch $arch_sim -isysroot $ios_sdk_dir -isysroot $ios_sdk_dir_sim -pipe -std=c99 -gdwarf-2"

  export LIBS="-framework Foundation"
}

select_ios_and_simulator

$ECL_IOS/../ecl-ios-host/bin/ecl -norc -shell $1

