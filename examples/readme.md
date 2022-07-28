
HowTo
-----

* to add a new app from the template, run:

```
$ ./copy.sh <name>
```

* if you want any of **Swank** / **ECL contrib files** / **QML auto reload** /
  a **trivial REPL** on mobile, just copy the `swank-server` example as your
  template

* for **QML single file auto reload** you want `advanced-qml-auto-reload`,
  which is derived from example `swank-server`



Note
----

Example [wear-os-gps](wear-os-gps/) is optimized for WearOS, but can also be
used on any phone. It demonstrates how to use the GPS sensor (including
permissions) on mobile.
