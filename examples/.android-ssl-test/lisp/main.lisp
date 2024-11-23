(in-package :app)

(defun ini ()
  (q> |text| ui:*label*
      (first (last (multiple-value-list (drakma:http-request "https://duckduckgo.com"))))))

(qlater 'ini)
