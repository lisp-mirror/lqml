
Build
-----

```
$ cd cpp
$ qmake lib.pro
$ make
$ cd ..
```


Run
---

```
$ lqml ~/slime/lqml-start-swank.lisp
$ emacs run.lisp
```

After `M-x slime-connect` and loading `run.lisp`, you can see that, despite
the argument and return type simply being defined as `QVariant`, you may also
pass lists, because a `QVariant` can also be of type `QVariantList`, so this
is a perfect fit for (nested) Lisp lists.

So, we pass a nested Lisp list, and it gets converted and shown on Qt side with
the respective types. Then the `QVariantList` is returned to Lisp, where it is
automatically converted back to a nested Lisp list.

Really convenient!

From the second function -- which calls back to Lisp -- we can see that it
suffices to simply pass some intuitive, primitive C++ values to `ecl_fun`,
which will be converted automatically (using `QVariant`) to the appropriate
Lisp values.

**Conclusion**: by only allowing `QVariant` arguments for calls between Lisp
and C++/Qt, we simplify things to a point where it becomes trivial, especially
considering nested lists on both sides.

