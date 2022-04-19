;;; original copyright: CL Maze 20030311 by Joe Wingbermuehle
;;;
;;; this is a reviewed version

(in-package :maze)

;; both must be odd
(defconstant *width*  25)
(defconstant *height* 39)

(defvar *maze*)

(defun carve-maze (x y)
  (let ((d (random 4)))
    (dotimes (c 4)
      (let* ((cd (mod (+ c d) 4))
             (dv (case cd
                   (0 (list 0 1))
                   (1 (list 1 0))
                   (2 (list -1 0))
                   (t (list 0 -1))))
             (x1 (+ x (car dv)))
             (y1 (+ y (cadr dv)))
             (x2 (+ x1 (car dv)))
             (y2 (+ y1 (cadr dv))))
        (when (and (< 0 x2 *width*)
                   (< 0 y2 *height*)
                   (aref *maze* x1 y1)
                   (aref *maze* x2 y2))
          (set-visible x1 y1 nil)
          (set-visible x2 y2 nil)
          (qsleep 0.001) ; slow down to make visible
          (carve-maze x2 y2))))))

(defun generate-maze ()
  (set-visible 1 1 nil)
  (carve-maze  1 1)
  (set-visible 1 0 nil)
  (set-visible (- *width* 1) (- *height* 2) nil))

(defun set-visible (x y visible)
  (setf (aref *maze* x y) visible)
  ;; |visible| doesn't work inside Repeater
  (q> |opacity| (q! |itemAt| ui:*maze*
                    (+ x (* y *width*)))
      (if visible 1 0)))

(defun display-maze ()
  (dotimes (y *height*)
    (dotimes (x *width*)
      (set-visible x y (aref *maze* x y)))))

(defun new-maze ()
  (setf *maze* (make-array (list *width* *height*)
                           :initial-element t))
  (display-maze)
  (qlater 'generate-maze))

(defun ini ()
  (setf *random-state* (make-random-state t))
  (new-maze))
