(defpackage :app
  (:use :cl :qml)
  (:export
   #:in-data-path))

(defpackage :db
  (:use :cl :qml)
  (:export
   #:delete-image
   #:delete-all-images
   #:ini
   #:load-images
   #:save-image
   #:size))

