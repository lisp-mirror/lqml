(in-package :maze)

(defconstant +w+ 13) ; ball width

(defvar *x*)         ; ball x
(defvar *y*)         ; ball y

(defvar *max-x* (* (1- *width*) +w+))

(defun move (x-rotation y-rotation) ; called from QML
  (labels ((add (x)
             (truncate (signum x)))
           (to-pos (x)
             (truncate (/ (+ x (/ +w+ 2))
                          +w+)))
           (normalize (x)
             (* (to-pos x) +w+)))
    ;; x axis rotation changes y
    ;; y axis rotation changes x
    (let* ((dx (min (* 0.2 y-rotation) (1- +w+)))
           (dy (min (* 0.2 x-rotation) (1- +w+)))
           (add-x (add dx))
           (add-y (add dy)))
      (setf *x* (min *x* *max-x*)) ; don't lose ball (initial x)
      (if (aref *maze*
                (+ add-x (to-pos *x*))
                (to-pos *y*))
          (setf *x* (normalize *x*))
          (incf *x* dx))
      (if (aref *maze*
                (to-pos *x*)
                (+ add-y (to-pos *y*)))
          (setf *y* (normalize *y*))
          (incf *y* dy))))
  (move-ball)
  (values)) ; no return value to QML

(defun move-ball ()
  (if (and (= *x* +w+)
           (= *y* 0))
      (new-game)
      (let ((ball (find-quick-item ui:*ball*)))
        ;; 'qset' is faster than 'q>', but can't trigger animations
        (qset ball |x| (- *x* 0.5))
        (qset ball |y| (- *y* 0.5)))))

(defun start ()
  (setf *x* (* +w+ (- *width*  1))
        *y* (* +w+ (- *height* 2)))
  (move-ball)
  (q> |running| ui:*timer* t))

(defun stop ()
  (q> |running| ui:*timer* nil))

(defun new-game ()
  (new-maze)
  (start))

(progn
  (start)
  (qlater 'ini))
