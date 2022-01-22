(defvar *cpp* (qload-c++ "cpp"))

(define-qt-wrappers *cpp*)

(print (hello *cpp* '(1 "two" (1.25 #(50 -50 75)))))

