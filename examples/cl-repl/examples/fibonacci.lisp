;;; fibonacci

(defun fib (n)
  (check-type n (integer 0 *))
  (labels ((fib-aux (n f1 f2)
             (if (zerop n)
                 f1
                 (fib-aux (1- n) f2 (+ f1 f2)))))
    (fib-aux n 0 1)))

