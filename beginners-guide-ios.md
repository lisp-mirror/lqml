
Preface
-------

Please note that it will take you probably several hours to setup everything
correctly, if you want to develop for mobile. Here you will find many links to
other places.


Xcode
-----

You should always use latest Xcode. If you want a specific version, see:
[Xcode Releases](https://xcodereleases.com). Prepare for a 10 GB download.
You'll need a (free) **Apple ID**, which is required anyway for deployment to
devices.


Install Qt5.15
--------------

For mobile we don't want latest Qt6, which currently still misses important QML
modules. There is basically no way around the online installer, because you
don't want to build the cross-compiled part of Qt for yourself.

See [reademe-qt](readme-qt.md) for the Qt online installer. You'll need to
register with an email.

Make sure to select the following in the online installer:

* [Qt5.15 Image Formats](doc/img/qt-image-formats.png)
* [Qt5.15 macOS, iOS](doc/img/qt-macos-ios.png)

You really want the online installer. Yes it's a 1 GB download, but it brings
all the examples, demos, tutorials, documentation, Qt Designer, all QML
modules, so you won't miss anything. And you can easily add/remove parts later
(see `~/Qt/MaintainanceTool` after installation), or install Qt6 in parallel in
the future.


Cross-compile ECL
-----------------

Next step is cross-compiling ECL. See
[readne-prepare-ios](readme-prepare-ios.md).


Build LQML
----------

Now build both the `lqml` executable and library, see
[readme-build](readme-build.md).

You may now try to run a desktop example, to see if it works. Every example has
a readme which explains everything.


Install example on iOS device
-----------------------------

The best example to take as a template for development on mobile is
[examples/advanced-qml-auto-reload](examples/advanced-qml-auto-reload).

Please see `readme` and `readme-build` of that example.
