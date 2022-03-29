(defpackage :cl-sokoban
  (:nicknames :sokoban)
  (:use :cl)
  (:export
   #:*mazes*
   #:*move-hook*
   #:*rules*
   #:*solutions*
   #:*undo-hook*
   #:copy-maze
   #:defmaze
   #:maze
   #:maze-dimensions
   #:maze-player
   #:maze-text
   #:move
   #:simple-ui
   #:undo))

(in-package :cl-sokoban)

(defvar *mazes* nil
  "A list of two-dimensional character arrays, describing Sokoban puzzles.")

(defvar *move-hook* nil)
(defvar *solutions* nil)

(defvar *rules*
  '(("@ " " @")
    ("@." " &")
    ("& " ".@")
    ("&." ".&")
    ("@$ " " @$")
    ("@$." " @*")
    ("&$ " ".@$")
    ("&$." ".@*")
    ("@* " " &$")
    ("@*." " &*")
    ("&* " ".&$")
    ("&*." ".&*"))
  "A list of textual transformation rules that the cl-sokoban mover steps
through.  A rule has the format (\"from\" \" to \"); when \"from\" matches
the maze, \" to \" replaces it in the maze.")

(defstruct (maze :named (:type vector) (:copier nil))
  player
  dimensions
  text)
(defun copy-maze (maze)
  (make-maze :player (maze-player maze)
             :dimensions (maze-dimensions maze)
             :text (mapcar #'copy-seq (maze-text maze))))

(defun simple-ui ()
  (do ((input "" (read-line)))
      ((search "q" input))
    (cond ((search "n" input) (move :north (first *mazes*)))
          ((search "e" input) (move :east (first *mazes*)))
          ((search "w" input) (move :west (first *mazes*)))
          ((search "s" input) (move :south (first *mazes*))))
    (format t "~{~&~A~%~}" (maze-text (first *mazes*)))))

(defun find-player (rows)
  (loop :for y :from 0
        :for row :in rows
        :for x? = (or (position #\@ row)
                      (position #\& row))
        :when x? return (cons x? y)
        :finally (error "Maze lacks a player (@): ~S" rows)))

(defun move (direction maze)
  (loop :for (from to) :in *rules*
        :when (string= from (lookahead (length from) direction maze))
        :do (return (setahead to direction maze))))

(defun move-point (location direction)
  (case direction
    (:east (cons (1+ (car location)) (cdr location)))
    (:west (cons (1- (car location)) (cdr location)))
    (:north (cons (car location) (1- (cdr location))))
    (:south (cons (car location) (1+ (cdr location))))))

(defun off-maze-p (location maze)
  (destructuring-bind (x . y) (maze-dimensions maze)
    (or (>= (car location) x)
        (>= (cdr location) y)
        (< (car location) 0)
        (< (cdr location) 0))))

(defun lookahead (distance direction maze)
  (do ((location (maze-player maze) (move-point location direction))
       (index 0 (1+ index))
       (chars nil (cons (elt (elt (maze-text maze) (cdr location))
                             (car location))
                        chars)))
      ((or (= index distance)
           (off-maze-p location maze))
       (coerce (reverse chars) 'string))))

(defun undo (maze steps)
  (dolist (step steps)
    (let* ((location (first step))
           (char (second step))
           (row (elt (maze-text maze) (cdr location))))
      (setf (elt row (car location)) char)))
  (setf (maze-player maze) (find-player (maze-text maze))))

(defun setahead (string direction maze)
  (let (undo-steps)
    (loop :for char :across string
          :for location = (maze-player maze)
          :then (prog1
                    (move-point location direction)
                  (when *move-hook*
                    (funcall *move-hook* char location direction)))
          :do (let ((row (elt (maze-text maze) (cdr location))))
                (when *undo-hook*
                  (push (list location (elt row (car location)))
                        undo-steps))
                (setf (elt row (car location)) char)))
    (when *undo-hook*
      (funcall *undo-hook* undo-steps)))
  (setf (maze-player maze) (find-player (maze-text maze))))

(defun defmaze (&rest rows)
  (push (make-maze :text rows
                   :dimensions (cons (length (first rows))
                                     (length rows))
                   :player (find-player rows))
        *mazes*))
