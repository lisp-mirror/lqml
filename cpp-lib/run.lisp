(defvar *cpp* (qload-c++ "cpp")) ; loads plugin in main/UI thread

(define-qt-wrappers *cpp*)

;; qrun* needed in Slime for message box (not running on UI thread)
(qrun* (print (hello *cpp* '(1 "two" (1.25 #(50 -50 75))))))

;; qrun* needed in Slime to see the return value (blocking call in main thread)
(qrun* (print (call-lisp *cpp* 42)))
