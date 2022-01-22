
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
$ emacs
```

In Slime do:
```
(defvar *cpp* (qload-c++ "cpp"))

(define-qt-wrappers *cpp*)

(hello *cpp* '(1 "two" (1.25 #(50 -50 75))))
```
Now look at the console output where you launched the `lqml` executable.

As you can see, although the argument and return type are simply defined as
`QVariant`, you may also pass lists, because a `QVariant` can also be of type
`QVariantList`, so this is a perfect fit for (nested) Lisp lists.

So, we pass a nested Lisp list, and it gets printed on Qt side with the
respective types. Then the `QVariantList` is returend to Lisp, where it is
automatically converted back to a nested Lisp list.

Realy convenient!

