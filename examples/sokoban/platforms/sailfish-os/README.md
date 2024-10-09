# Example Dockerfile to Build Sokoban for AARCH64

## Dependency

This project depends on [SFOSBID](https://codeberg.org/aerique/sfosbid).
This Git repository needs to be cloned somewhere and then, in the root of
that project's directory, the following commands should be issued:

- `bin/download.sh`
- `bin/build.sh`

Once this has been successfully built we can continue with building the
Sokoban RPM.

## Building the Sokoban RPM

Go to the LQML project's root directory, then:

- `cd examples/sokoban`
- `docker build -t sokoban -f platforms/sailfish-os/Dockerfile .`
    - this takes a long time, especially if one is on another architecture
      than the one being built
- `docker run -it sokoban`

A message will be displayed on how to copy the RPM out of the running
container.

## Installing the Sokoban RPM

Get the RPM onto your Sailfish OS phone either by using SSH, by putting it
online somewhere, emailing it to yourself, etc.

Then either in Downloads, Transfers or using the file manager install the RPM.

There should now be an new icon in your apps menu called "Sokoban".

## Building for other architectures

All relevant command are called with `sb2 -t SailfishOS-latest-aarch64 …`,
so creating a new Dockerfile or replacing `SailfishOS-latest-aarch64` with
one of the other installed architectures this project should build for that
architecture.

Available architectures in [SFOSBID](https://codeberg.org/aerique/sfosbid):

- `aarch64`
- `armv7hl` (this is the old 32-bit Sailfish OS architecture)
- `i486`

Building for the other architectures has not been tested.
