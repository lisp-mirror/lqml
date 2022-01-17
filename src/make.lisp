(require :asdf)

(push (merge-pathnames "../")
      asdf:*central-registry*)

(asdf:make-build "lqml"
                 :monolithic t
                 :type :static-library
                 :move-here "./"
		 :init-name "ini_LQML")

(let ((from "lqml--all-systems.a")
      (to "liblqml.a"))
  (when (probe-file to)
    (delete-file to))
  (rename-file from to))

