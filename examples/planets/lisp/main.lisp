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
;;; The model keys (like ':m-name') are all preceeded by 'm-' to prevent
;;; eventual name clashes with property names in QML. The 'm' stands for
;;; 'model'. This would not be a problem in this example, but it's a good
;;; generic rule.

(defun populate-item-model ()
  (q! |clear| *planets*)
  (qjs |addPlanet| *planets*
       (list :m-name  "Sun"
             :m-shape "img/sun.png"
             :m-map   "img/sun-map.jpg"
             :m-info  #.(read-file "txt/sun.txt")))
  (qjs |addPlanet| *planets*
       (list :m-name  "Mercury"
             :m-shape "img/mercury.png"
             :m-map   "img/mercury-map.jpg"
             :m-info  #.(read-file "txt/mercury.txt")))
  (qjs |addPlanet| *planets*
       (list :m-name  "Venus"
             :m-shape "img/venus.png"
             :m-map   "img/venus-map.jpg"
             :m-info  #.(read-file "txt/venus.txt")))
  (qjs |addPlanet| *planets*
       (list :m-name  "Earth"
             :m-shape "img/earth.png"
             :m-map   "img/earth-map.jpg"
             :m-info  #.(read-file "txt/earth.txt")))
  (qjs |addPlanet| *planets*
       (list :m-name  "Moon"
             :m-shape "img/moon.png"
             :m-map   "img/moon-map.jpg"
             :m-info  #.(read-file "txt/moon.txt")))
  (qjs |addPlanet| *planets*
       (list :m-name "Mars"
             :m-shape "img/mars.png"
             :m-map   "img/mars-map.jpg"
             :m-info #.(read-file "txt/mars.txt")))
  (qjs |addPlanet| *planets*
       (list :m-name  "Jupiter"
             :m-shape "img/jupiter.png"
             :m-map   "img/jupiter-map.jpg"
             :m-info  #.(read-file "txt/jupiter.txt")))
  (qjs |addPlanet| *planets*
       (list :m-name  "Saturn"
             :m-shape "img/saturn.png"
             :m-map   "img/saturn-map.jpg"
             :m-info  #.(read-file "txt/saturn.txt")))
  (qjs |addPlanet| *planets*
       (list :m-name  "Uranus"
             :m-shape "img/uranus.png"
             :m-map   "img/uranus-map.jpg"
             :m-info  #.(read-file "txt/uranus.txt")))
  (qjs |addPlanet| *planets*
       (list :m-name  "Neptune"
             :m-shape "img/neptune.png"
             :m-map   "img/neptune-map.jpg"
             :m-info  #.(read-file "txt/neptune.txt"))))

(populate-item-model)
