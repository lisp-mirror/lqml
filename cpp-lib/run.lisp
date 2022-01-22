(defvar *cpp* (qload-c++ "cpp"))

(define-qt-wrappers *cpp*)

;; qrun* needed in Slime (not running on UI thread)

(qrun* (print (hello *cpp* '(1 "two" (1.25 #(50 -50 75))))))
