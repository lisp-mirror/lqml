(in-package :app)

;;; QML items

(defvar *planets* "planets") ; ListModel

;;; We can't call 'planets.append()' directly from Lisp (because its argument
;;; type is a JS object pointer); instead we use a trivial JS glue code
;;; function. The advantage of calling a user defined JS function is that we
;;; can directly pass (nested) Lisp lists.
;;;
;;; A special case is a Lisp list whose first element is a Lisp keyword, like
;;; below. In this case, the list is converted to a QVariantMap, which on JS
;;; side is equal to a JS dictionary, and can therefore directly be used to
;;; populate an item model.
;;;
;;; QML methods which only expect primitive argument types can of course be
;;; called directly, for example:
;;;
;;;   (q! |remove| *planets* 0)
;;;   (q! |clear| *planets*)
;;;   (q! |setProperty| *planets* 0 "name" "First")
;;;
;;; In QML you should always use the 'model.' namespace for the keys to prevent
;;; eventual name clashes: 'text: model.text'

(defun add-planet (planet)
  (qjs |addPlanet| *planets* planet))

(defun populate-item-model ()
  (q! |clear| *planets*)
  (add-planet
   (list :name  "Sun"
         :shape "img/sun.png"
         :map   "img/sun-map.jpg"
         :info  #.(read-file "txt/sun.txt")))
  (add-planet
   (list :name  "Mercury"
         :shape "img/mercury.png"
         :map   "img/mercury-map.jpg"
         :info  #.(read-file "txt/mercury.txt")))
  (add-planet
   (list :name  "Venus"
         :shape "img/venus.png"
         :map   "img/venus-map.jpg"
         :info  #.(read-file "txt/venus.txt")))
  (add-planet
   (list :name  "Earth"
         :shape "img/earth.png"
         :map   "img/earth-map.jpg"
         :info  #.(read-file "txt/earth.txt")))
  (add-planet
   (list :name  "Moon"
         :shape "img/moon.png"
         :map   "img/moon-map.jpg"
         :info  #.(read-file "txt/moon.txt")))
  (add-planet
   (list :name  "Mars"
         :shape "img/mars.png"
         :map   "img/mars-map.jpg"
         :info  #.(read-file "txt/mars.txt")))
  (add-planet
   (list :name  "Jupiter"
         :shape "img/jupiter.png"
         :map   "img/jupiter-map.jpg"
         :info  #.(read-file "txt/jupiter.txt")))
  (add-planet
   (list :name  "Saturn"
         :shape "img/saturn.png"
         :map   "img/saturn-map.jpg"
         :info  #.(read-file "txt/saturn.txt")))
  (add-planet
   (list :name  "Uranus"
         :shape "img/uranus.png"
         :map   "img/uranus-map.jpg"
         :info  #.(read-file "txt/uranus.txt")))
  (add-planet
   (list :name  "Neptune"
         :shape "img/neptune.png"
         :map   "img/neptune-map.jpg"
         :info  #.(read-file "txt/neptune.txt"))))

(populate-item-model)
