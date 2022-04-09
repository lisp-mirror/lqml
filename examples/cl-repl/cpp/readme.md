
Info
----

This demonstrates how to extend your app with Qt code callable from Lisp.

Please note that we only need `QVariant` arguments, which can also hold
pointers which are type checked when retrieving the pointer value (that is,
the pointer is a `nullptr` if the type doesn't match).

It also shows how to extend non `QObject` classes with a `QObject`, in order
to store their pointer values in a `QVariant`.

If we need to return a `QObject` value (not a pointer or primitive value) to
Lisp, it's simply created on the heap, calling `deleteLater()` on it, which
should be sufficient in most cases.
