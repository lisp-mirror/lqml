(in-package :pal)

(defun item-count ()
  (loop :for item :in *items*
        :sum (length (second item))))

(defvar *chars* (loop :for i :below (item-count) :collect (code-char (+ i #.(char-code #\a)))))

(defun image-of-char (char)
  (dolist (item *items*)
    (x:when-it (find char (second item))
      (return-from image-of-char (first item)))))

(defun compute-move-to-positions ()
  (flet ((item-pos (char list)
           (let ((y 0))
             (dolist (state list)
               (incf y)
               (x:when-it (position char state)
                 (return-from item-pos (list (1+ x:it) y)))))))
    (let (states)
      (dolist (state *states*)
        (let (positions)
          (dolist (char *chars*)
            (push (item-pos char state)
                  positions))
          (push (nreverse positions) states)))
      (nreverse states))))

(defvar *move-to-positions* (compute-move-to-positions))
