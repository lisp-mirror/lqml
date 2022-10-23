
Info
----

This demonstrates how to extend your app with Qt code callable from Lisp.

Please note that we only need `QVariant` arguments, which can also hold
pointers which are type checked when retrieving the pointer value (that is,
the pointer is a `nullptr` if the type doesn't match).

It also shows how to extend non `QObject` classes with a `QObject`, in order
to store their pointer values in a `QVariant`.

If we need to return a non `QObject` value to Lisp (not a pointer or primitive
value, but a class like `TextBlock` in this example, that is a `QTextBlock`
extended with a `QObject`), a new instance is created on the heap, calling
`QTimer::singleShot(0, tmp, &QObject::deleteLater)` on it, which should be
sufficient in any circumstance. The additional timer is needed here because of
`SplitView`, which delays certain events.
