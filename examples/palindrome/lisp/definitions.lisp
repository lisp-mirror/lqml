(in-package :pal)

(defparameter *items*
  '(("P" "gs")
    ("A" "djpv")
    ("T" "ckow")
    ("E" "hlnr")
    ("R" "aiqy")
    ("N" "m")
    ("O" "bftx")
    ("S" "eu")))

(defparameter *state-1*
  '("..............."
    "..............."
    "..............."
    "..............."
    "..............."
    ".....abcde....."
    ".....fghij....."
    ".....klmno....."
    ".....pqrst....."
    ".....uvwxy....."
    "..............."
    "..............."
    "..............."
    "..............."
    "..............."))

(defparameter *state-2*
  '(".......d......."
    "..............."
    ".......g......."
    ".......j......."
    ".......c......."
    ".......h......."
    ".......a......."
    "p.svklqmbeoni.t"
    ".......f......."
    ".......u......."
    ".......w......."
    ".......r......."
    ".......y......."
    "..............."
    ".......x......."))

(let (ex)
  (defun rotated (&optional (state ex))
    (let* ((width  (length (first state)))
           (height (length state))
           (array  (make-array (list width height))))
      (loop :for string :in state
            :for y :upfrom 0
            :do (loop :for ch :across string
                      :for x :upfrom 0
                      :do (setf (aref array x (- (1- width) y)) ; rotate
                                ch)))
      (setf ex (loop :for y :below height
                     :collect (coerce (loop :for x :below width
                                            :collect (aref array y x))
                                      'string)))))
  (defun ex-rotated ()
    ex))

(defparameter *states*
  (list *state-1*           ; 1
        *state-2*
        *state-1*
        (rotated *state-1*) ; 2
        *state-2*
        (ex-rotated)
        (rotated)           ; 3
        *state-2*
        (ex-rotated)
        (rotated)           ; 4
        *state-2*
        (ex-rotated)))
