(defpackage :kalman
  (:use :cl)
  (:nicknames :kal)
  (:export
   #:*lat*
   #:*lon*
   #:*timestamp*
   #:*variance*
   #:filter
   #:reset))

(in-package :kal)

(defvar *lat*       nil)
(defvar *lon*       nil)
(defvar *timestamp* nil)
(defvar *variance*  nil)

(defun reset ()
  (setf *variance* nil))

(defun filter (lat lon accuracy speed)
  (when (and lat lon accuracy speed)
    (setf accuracy (max 1 accuracy))
    (let ((timestamp (get-internal-real-time)))
      (if *variance*
          (let ((time-inc (- timestamp *timestamp*)))
            (incf *variance* (/ (* time-inc (expt speed 2)) 1000))
            (setf *timestamp* timestamp)
            (let ((k (/ *variance* (+ *variance* (expt accuracy 2)))))
              (incf *lat* (* k (- lat *lat*)))
              (incf *lon* (* k (- lon *lon*)))
              (setf *variance* (* (- 1 k) *variance*))))
          (setf *lat*       lat
                *lon*       lon
                *timestamp* timestamp
                *variance*  (expt accuracy 2))))))
