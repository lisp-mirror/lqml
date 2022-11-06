(defpackage :camera
  (:use :cl :qml :s-http-server)
  (:export
   #:*image-path*
   #:*web-server*
   #:create-index.html))

