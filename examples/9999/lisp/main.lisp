(in-package :qml-user)

(defvar *number* 0)

;;; QML items

(defvar *canvas* "canvas")
(defvar *input*  "input")

(defun draw-line (x1 y1 x2 y2)
  (qjs |drawLine| *canvas*
       x1 y1 x2 y2))

(defun draw-number (number)
  (setf *number* number)
  (q! |requestPaint| *canvas*))

(defun paint ()
  (draw-line 0 -150 0 150)
  (let ((dy -50)
        (dig 1))
    (labels ((line (x1 y1 x2 y2)
               (when (find dig '(2 4))
                 (setf x1 (- x1)
                       x2 (- x2)))
               (when (>= dig 3)
                 (setf y1 (- y1)
                       y2 (- y2)
                       dy 50))
               (draw-line (* 100 x1) (+ dy (* 100 y1))
                          (* 100 x2) (+ dy (* 100 y2))))
             (draw (n)
               (case n
                 (1 (line 0 -1  1 -1))
                 (2 (line 0  0  1  0))
                 (3 (line 0 -1  1  0))
                 (4 (line 0  0  1 -1))
                 (5 (draw 1) (draw 4))
                 (6 (line 1 -1  1  0))
                 (7 (draw 1) (draw 6))
                 (8 (draw 2) (draw 6))
                 (9 (draw 1) (draw 8)))))
      (let ((num *number*))
        (x:while (plusp num)
          (draw (mod num 10))
          (setf num (floor (/ num 10)))
          (incf dig))))))

