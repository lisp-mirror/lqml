(in-package :qml-user)

;; set here both function and range

(defparameter *fun*
  (lambda (x)
    (+ 2 (- (expt x 3)
            (* x 3)))))

(defparameter *range* (list -3 3))
(defparameter *zero*  t)

(defvar *canvas* "canvas")

(defun line (x1 y1 x2 y2)
  (qjs |line| *canvas* x1 y1 x2 y2))

(defun move-to (x y)
  (qjs |moveTo| *canvas* x y))

(defun line-to (x y)
  (qjs |lineTo| *canvas* x y))

(defmacro with-path ((color line-width) &body body)
  `(progn
     (qjs |begin| *canvas* ,color ,line-width)
     ,@body
     (qjs |end| *canvas*)))

(defun paint () ; called from QML
  "Draw function graph, adapting the maximum y value to the available canvas height."
  (let* ((y-max 0)
         (w-can (q< |width| *canvas*))
         (y-can (/ (q< |height| *canvas*) 2))
         (x0 (first *range*))
         (dx (/ (- (second *range*) x0) w-can)))
    (labels ((fun (x)
               (- (funcall *fun* (+ x0 (* x dx)))))
             (draw (x)
               (* (/ y-can y-max) (fun x))))
      (dotimes (x (1+ w-can))
        (setf y-max (max (abs (fun x)) y-max)))
      (when *zero*
        (with-path ("gray" 1)
          (line 0 0 w-can 0)))
      (with-path ("blue" 2)
        (move-to 0 (draw 0))
        (dotimes (i w-can)
          (let ((x (1+ i)))
            (line-to x (draw x))))))))
