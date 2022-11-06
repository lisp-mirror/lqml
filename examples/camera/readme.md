
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

You may wonder about the "wrong" orientation of the images in the browser: this
depends both on the camera orientation and on the mobile OS.



Run
---
```
lqml run.lisp
```
Optionally pass `-slime` to start a Swank server, and connect from Emacs with
`M-x slime-connect`.

During development you can pass `-auto`, which will releoad all QML files after
you made a change to any of them and saved it. For re-initialization after
reloading, file `lisp/qml-reload/on-reloaded` will be loaded.

Closing the window quits the app. If you try to kill it with `ctrl-c`, you need
an additional `ctrl-d` to exit from ECL. To quit from Slime, do `(qq)` which is
short for `(qquit)`.

