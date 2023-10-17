(defpackage :gauge
  (:use :cl :qml)
  (:export
   #:*gauge*
   #:paint
   #:number-pos
   #:ini))

(in-package :gauge)

(defvar *gauge*          "gauge")
(defvar *canvas*         "gauge_canvas")
(defvar *numbers-loader* "gauge_numbers_loader")

(defun set-style (color width)
  (qjs |setStyle| *canvas*
       color (float width)))

(defun draw-line (x1 y1 x2 y2)
  (qjs |drawLine| *canvas*
       (float x1) (float y1) (float x2) (float y2)))

(defun rotate (angle)
  (qjs |rotate| *canvas*
       (float angle)))

(defun arc (x y r start end)
  (qjs |arc| *canvas*
       (float x) (float y) (float r) (float start) (float end)))

(defmacro with-path (&body body)
  `(progn
     (qjs |beginPath| *canvas*)
     ,@body
     (qjs |stroke| *canvas*)))

(defmacro with-save (&body body)
  `(progn
     (qjs |save| *canvas*)
     ,@body
     (qjs |restore| *canvas*)))

(defun to-rad (deg)
  (/ (* deg pi) 180))

(defun paint () ; called from QML
  (let ((r (q< |r| *gauge*))
        (f (q< |f| *gauge*)))
    ;; limit zone
    (set-style "red" (* 12 f))
    (with-path ()
      (arc 0 0 (- r (* 6 f))
           (to-rad (- 360
                      (* 180 (- 1 (q< |limit| *gauge*)))))
           (to-rad 360)))
    ;; thin ticks
    (set-style "white" (* 2 f))
    (with-save ()
      (with-path ()
        (rotate (to-rad 90))
        (dotimes (i 60)
          (draw-line 0 (- r (* 5 f)) 0 r)
          (rotate (to-rad 3)))))
    ;; main ticks
    (set-style "white" (* 4 f))
    (with-path ()
      (rotate (to-rad 90))
      (dotimes (i 11)
        (draw-line 0 (- r (* 12 f)) 0 r)
        (rotate (to-rad 18))))))

(defun number-pos (xy index) ; called from QML
  (let ((x? (string= "x" xy))
        (r (q< |r| *gauge*))
        (f (q< |f| *gauge*)))
    (funcall (if x? '+ '-)
             r
             (* (- r (* 34 f))
                (funcall (if x? 'sin 'cos)
                         (to-rad (+ 270 (* index 18))))))))

(defun ini ()
  ;; needs to be delayed because of recursive dependency:
  ;; - QML is loaded before Lisp
  ;; - 'x' and 'y' positions are calculated in Lisp
  (q> |active| *numbers-loader* t))

(qlater 'ini)
