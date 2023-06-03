(in-package :msg)

(defvar *messages*   nil)
(defvar *message-id* 0)
(defvar *states*     '(:not-received :out-there :received))

(defun add-message (message &optional loading)
  "Adds passed MESSAGE (a PLIST) to both the QML item model and *MESSAGES*.
  The model keys are:
  :m-text :m-sender :m-timestamp :m-id :m-ack-state"
  (qjs |addMessage| ui:*messages* message)
  (unless loading
    (push message *messages*)
    (qlater 'save-messages)))

(defun change-state (state id)
  (let ((i-state (position state *states*)))
    (qjs |changeState| ui:*messages*
         i-state id)
    (dolist (msg *messages*)
      (when (eql (getf msg :m-id) id) ; EQL: might be NIL
        (setf (getf msg :m-ack-state) i-state)
        (return))))
  (qlater 'save-messages))

(defvar *file* (merge-pathnames "data/messages.exp"))

(defun load-messages ()
  "Loads *MESSAGES* which can directly be passed to the QML item model."
  (when (probe-file *file*)
    (with-open-file (s *file*)
      (setf *messages* (read s)))
    (dolist (msg (reverse *messages*))
      (setf *message-id* (max (or (getf msg :m-id) 0)
                              *message-id*))
      (add-message msg t))))

(defun save-messages ()
  "Saves *MESSAGES* by simply printing them into a file."
  (ensure-directories-exist *file*)
  (with-open-file (s *file* :direction :output :if-exists :supersede)
    (let ((*print-pretty* nil))
      (prin1 *messages* s))))

