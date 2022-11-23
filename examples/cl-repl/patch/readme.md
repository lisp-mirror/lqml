
Info
----

This is a small patch to make `:zip` from Quicklisp work with ECL (or any CL
implementation, for that matter).

It uses a trivial approach by simply applying a stat of `644` for files, and
`775` for directories (that is, files with a `pathname-type` of `NIL`).



HowTo
-----

Copy project `:zip` from Quicklisp to `~/quicklisp/local-projects/` and patch
file `zip.lisp` according to `zip.diff`.
