(defpackage :app
  (:use :cl :qml)
  (:export))

;; hack, loads empty system to suppress ASDF runtime error "system not found"
(progn
  (push *default-pathname-defaults* asdf:*central-registry*)
  (asdf:load-system :clog))
