
Prepare
-------

Please copy the app template files first:
```
$ cd ..
$ ./copy.sh camera
```


Info
----

This shows how to use the QML `Camera` item. Only very basic usage is covered
here -- of course you can set many parameters, if you want to dive into all the
details (see Qt Assistant).

Additionally, a basic web-server is integrated, in order to watch/download the
images (mobile device) on the desktop computer. On every new image taken, the
`index.html` is updated.

To display the images on the desktop, enter the following link, using the IP of
the mobile device (assuming both desktop and mobile are in the same WiFi):

```
http://192.168.1.x:1701/
```
**1701** is the default port of `:s-http-server` (from Quicklisp), which was
chosen here because it's both relatively small and works well on mobile.

This example needs a small Qt extension for rotating images. That's necessary
here because different devices may have different orientation handling (namely
iOS), so the saved images would be displayed with the wrong orientation in the
desktop browser.



Note
----

While testing on different devices (phone/tablet), I discovered that they may
behave differently regarding the saved image orientation, even if running the
same OS. So, to adapt to your device's behavior, you need to play around with
`rotation` in `updateImageRotation()` (QML). See also `qt:rotate-image` (Lisp).

The current settings worked for me on both an android phone and an iPhone (both
older models).



Run
---
```
lqml run.lisp
```
Optionally pass `-slime` to start a Swank server, and connect from Emacs with
`M-x slime-connect`.

During development you can pass `-auto`, which will reload all QML files after
you made a change to any of them and saved it. For re-initialization after
reloading, file `lisp/qml-reload/on-reloaded` will be loaded.

Closing the window quits the app. If you try to kill it with `ctrl-c`, you need
an additional `ctrl-d` to exit from ECL. To quit from Slime, do `(qq)` which is
short for `(qquit)`.

