(in-package :app)

#+android
(when (probe-file "libssl.so")
  (ffi:load-foreign-library "libcrypto.so")
  (ffi:load-foreign-library "libssl.so"))

(defun ini ()
  (q> |text| ui:*label*
      (first (last (multiple-value-list (drakma:http-request "https://duckduckgo.com"))))))

(qlater 'ini)
