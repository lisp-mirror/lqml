
Build
-----

Switch to `cpp/` and do:
```
qmake lib.pro
make
```


Run
---

```
$ lqml ~/slime/qml-start-swank.lisp

$ emacs run.lisp
```

After `M-x slime-connect` and loading `run.lisp`, you can see that, despite
the argument and return type simply being defined as `QVariant`, you may also
pass lists, because a `QVariant` can also be of type `QVariantList`, so this
is a perfect fit for (nested) Lisp lists.

So, we pass a nested Lisp list, and it gets shown on Qt side with the
respective types. Then the `QVariantList` is returned to Lisp, where it is
automatically converted back to a nested Lisp list.

Really convenient!

