
Prepare
-------

Please copy the app template files first:
```
$ cd ..
$ ./copy.sh swank-server
```

See also [../../slime/src/readme-sources](../../slime/src/readme-sources.md) for
installing the Slime sources where this example can find them.



Info
----

Meant for mobile only. Provides both a **Swank server** and **Quicklisp**.

A simple REPL is integrated in order to start Swank with `:s`, and enable
Quicklisp with `:q`.

The most convenient way to connect from Slime is entering the IP address of the
mobile device after `M-x slime-connect`.



Quicklisp note
--------------

On **andoid**, Quicklisp is directly downloaded and installed.

On **iOS** the above method would crash ECL; this seems to be caused by the
limited stack size on iOS.

So, in order to use Quicklisp on iOS, you need to bring it yourself, which
means:

* install a fresh copy of Quicklisp on some desktop device, and copy over the
  whole directory `quicklisp` under `platforms/ios/assets/Library/` of this
  example

* after installing the app on the iOS device, command `:q` will then simply
  load `quicklisp/setup` from the locally installed version
