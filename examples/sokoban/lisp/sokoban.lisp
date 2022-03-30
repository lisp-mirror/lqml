;;;
;;; QML UI for CL-Sokoban, see http://www.cliki.net/CL-Sokoban 
;;;

(in-package :qsoko)

(defvar *item-types*
  '((#\# . :wall)
    (#\$ . :object)
    (#\* . :object2)
    (#\. . :goal)
    (#\@ . :player)
    (#\& . :player2)))

(defvar *items*         nil)
(defvar *item-size*     nil)
(defvar *maze*          nil)
(defvar *my-mazes*      (mapcar 'sokoban:copy-maze sokoban:*mazes*))
(defvar *solving*       nil)
(defvar *undo-stack*    nil)
(defvar *level-changed* nil)

(defun level ()
  (floor (q< |value| ui:*level*)))

(defun set-level (index)
  (q> |value| ui:*level* index))

(defun assoc* (item alist)
  (cdr (assoc item alist)))

(defun char-type (char)
  (cdr (assoc char *item-types*)))

(defun type-char (type)
  (car (find type *item-types* :key 'cdr)))

(defun set-maze ()
  (setf *maze* (nth (level) *my-mazes*))
  (update-translate-xy)
  (create-items)
  (place-all-items)
  (setf *undo-stack* nil))

(defun reset-maze ()
  (setf *maze* (setf (nth (level) *my-mazes*)
                     (sokoban:copy-maze (nth (level) sokoban:*mazes*))))
  (update-placed-items t)
  (setf *undo-stack* nil))

(defvar *translate-x* 0)
(defvar *translate-y* 0)

(defun find-file (file)
  (x:if-it (probe-file file)
           (format nil "file://~A" x:it)
           (x:cc "qrc:/" file)))

(defun update-translate-xy ()
  "Set x and y translation for maze centering."
  (let ((dim (sokoban:maze-dimensions *maze*))
        (img-px 32)
        (board-size 16))
    (setf *translate-x* (floor (/ (* img-px (- board-size (car dim))) 2))
          *translate-y* (floor (/ (* img-px (- board-size (cdr dim))) 2)))))
  
(defun create-item (type)
  (let* ((name (string-downcase type))
         (item (qjs |createItem| ui:*dynamic* name)))
    (q> |source| item
        (find-file (format nil "qml/img/~A.png" name)))
    (q> |objectName| item name)
    (unless *item-size*
      (setf *item-size* (q< |sourceSize| item)))
    item))

(defun create-items ()
  (clear-items)
  (flet ((add (types)
           (dolist (type (x:ensure-list types))
             (let ((item (create-item type)))
               (push item (cdr (assoc type *items*)))
               ;; add to QObject hirarchy, for 'objectName' to be findable
               (qset item |parent|
                     (find-quick-item ui:*board*))))))
    (dolist (row (sokoban:maze-text *maze*))
      (x:do-string (char row)
        (unless (char= #\Space char)
          (let ((type (char-type char)))
            (cond ((find type '(:player :player2))
                   (add '(:player :player2)))
                  ((find type '(:object :object2))
                   (add '(:object :object2 :goal)))
                  ((eql :wall type)
                   (add :wall)))))))))

(defvar *no-delete* nil)

(defun clear-items ()
  (unless *no-delete*
    (dolist (items *items*)
      (dolist (item (rest items))
        (q! |destroy| item))))
  (setf *items* (mapcar (lambda (x) (list (cdr x))) *item-types*)))

(defvar *running-animations* 0)
(defvar *function-queue*     nil)

(defun animation-change (running) ; called from QML
  (incf *running-animations* (if running 1 -1))
  (x:while (and (zerop *running-animations*)
                *function-queue*)
    (funcall (pop *function-queue*))))

(defun run-or-enqueue (function)
  (if (zerop *running-animations*)
      (funcall function)
      (setf *function-queue* (nconc *function-queue* (list function)))))

(defmacro queued (&rest functions)
  "Run passed functions in order, waiting for currently running (or newly
  triggered) animations to finish first."
  `(progn
     ,@(mapcar (lambda (fun) `(run-or-enqueue (lambda () ,fun)))
               functions)))

(defun change-level (direction/index)
  "Changes *LEVEL* in given direction or to index."
  (let ((level (min (1- (length *my-mazes*))
                    (max 0 (if (numberp direction/index)
                               direction/index
                               (+ (if (eql :next direction/index) 1 -1)
                                  (level)))))))
    (when (/= level (level))
      (queued (q> |running| ui:*zoom-board-out* t)
              (set-level level) ; will call SET-MAZE from QML
              (q> |running| ui:*zoom-board-in* t))))
  (setf *level-changed* t)
  (level))

(defun solve ()
  (setf *level-changed* nil)
  (let ((*solving* t))
    (reset-maze)
    (x:do-string (ch (nth (level) sokoban:*solutions*))
      (when *level-changed*
        (return-from solve))
      (sokoban:move (case (char-downcase ch)
                      (#\u :north)
                      (#\d :south)
                      (#\l :west)
                      (#\r :east))
                    *maze*)
      (x:while (plusp *running-animations*)
        (qsleep 0.05)))))

(defun set-x (item x &optional animate)
  (let ((x* (+ x *translate-x*)))
    (if animate
        (q> |x| item x*)
        (qset item |x| x*))))

(defun set-y (item y &optional animate)
  (let ((y* (+ y *translate-y*)))
    (if animate
        (q> |y| item y*)
        (qset item |y| y*))))

(defun child-at (x y)
  (q! |childAt| ui:*board*
      (+ x *translate-x*)
      (+ y *translate-y*)))

(defun place-items (type &optional reset)
  (let ((char (type-char type))
        (items (assoc* type *items*))
        (y 0))
    (unless (eql :wall type)
      (dolist (item items)
        (q> |visible| item nil)))
    (dolist (row (sokoban:maze-text *maze*))
      (let ((x 0))
        (x:do-string (curr-char row)
          (when (char= char curr-char)
            (let ((item (first items)))
              (set-x item x)
              (set-y item y)
              (q> |visible| item t))
            (setf items (rest items)))
          (incf x (first *item-size*))))
      (incf y (second *item-size*)))))

(defun place-all-items ()
  (dolist (type '(:wall :goal :object2 :player2 :player :object))
    (place-items type)))

(defun update-placed-items (&optional reset)
  (dolist (type '(:goal :object2 :player2 :player :object))
    (place-items type reset)))

(let (ex ex-ex)
  (defun move-item (char pos direction) ; see sokoban:*move-hook*
    (let* ((type (char-type char))
           (pos-x (car pos))
           (pos-y (cdr pos))
           (w (first *item-size*))
           (h (second *item-size*))
           (x (* w pos-x))
           (y (* h pos-y))
           (dx (case direction (:east w) (:west (- w)) (t 0)))
           (dy (case direction (:south h) (:north (- h)) (t 0)))
           (item (child-at (+ x (/ w 2)) (+ y (/ h 2)))))
      (unless (qnull item)
        (if (zerop dy)
            (set-x item (+ x dx) 'animate)
            (set-y item (+ y dy) 'animate))
        (dolist (tp (list type ex ex-ex))
          (when (find tp '(:player2  :object2 :goal))
            (queued (update-placed-items))
            (return)))
        (shiftf ex-ex ex type)
        (when (eql :player type)
          (qlater (lambda () (when (game-finished)
                               (final-animation)))))))))

(defun add-undo-step (step)
  (push step *undo-stack*))

(defun undo ()
  (when *undo-stack*
    (sokoban:undo *maze* (pop *undo-stack*))
    (update-placed-items)))

(defun game-finished ()
  ;; finished: no more :object, only :object2
  (let ((ch (type-char :object)))
    (dolist (str (sokoban:maze-text *maze*))
      (when (find ch str)
        (return-from game-finished))))
  t)

(defun final-animation ()
  (queued (q> |running| ui:*rotate-player* t)
          (q>* |running| ui:*wiggle-box* t)))

(defun button-pressed () ; called from QML
  (let ((button (intern (string-upcase (q< |objectName| *caller*))
                        :keyword)))
    (case button
      (:up       (sokoban:move :north *maze*))
      (:down     (sokoban:move :south *maze*))
      (:left     (sokoban:move :west *maze*))
      (:right    (sokoban:move :east *maze*))
      (:previous (change-level :previous))
      (:next     (change-level :next))
      (:undo     (undo))
      (:restart  (reset-maze))
      (:solve    (qlater 'solve)))) ; QLATER: prevent timer problem
  (values)) ; no return value to QML

(defun start ()
  (setf sokoban:*move-hook* 'move-item
        sokoban:*undo-hook* 'add-undo-step)
  (q> |to| ui:*level* (1- (length *my-mazes*)))
  (set-maze))

(qlater 'start)
