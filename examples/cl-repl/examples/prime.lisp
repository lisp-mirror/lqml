;;; primes

(defun primep (x)
  (or (= 2 x)
      (and (integerp x)
           (> x 2)
           (not (zerop (mod x 2)))
           (loop :for i :from 3 :to (isqrt x) :by 2
             :never (zerop (mod x i))))))
