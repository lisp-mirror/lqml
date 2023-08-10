;;; This is included here because we don't want to depend on the
;;; 'protoc-gen-cl-pb' executable, so we include the already generated lisp
;;; files from the proto files of both cl-protobufs and meshtastic.
;;;
;;; So, this is just the strict minimum necessary for being able to serialize
;;; and deserialize meshtastic data.
;;;
;;; It also has a bug fix in file 'lisp/cl-protobufs/wire-format', function
;;; 'decode-single', which needs investigation (whose bug is it?).

(defsystem :my-cl-protobufs
  :serial t
  :depends-on (:closer-mop
               :babel
               :alexandria
               :trivial-garbage
               :cl-base64
               :local-time
               :float-features)
  :components (;; cl-protobufs
               (:file "lisp/cl-protobufs/pkgdcl")
               (:file "lisp/cl-protobufs/utilities")
               (:file "lisp/cl-protobufs/model-classes")
               (:file "lisp/cl-protobufs/conditions")
               (:file "lisp/cl-protobufs/parser")
               (:file "lisp/cl-protobufs/define-proto")
               (:file "lisp/cl-protobufs/buffers")
               (:file "lisp/cl-protobufs/text-format")
               (:file "lisp/cl-protobufs/wire-format")
               ;; cl-proto
               (:file "lisp/proto/cl-proto/any")
               (:file "lisp/proto/cl-proto/source-context")
               (:file "lisp/proto/cl-proto/type")
               (:file "lisp/proto/cl-proto/api")
               (:file "lisp/proto/cl-proto/duration")
               (:file "lisp/proto/cl-proto/empty")
               (:file "lisp/proto/cl-proto/field-mask")
               (:file "lisp/proto/cl-proto/struct")
               (:file "lisp/proto/cl-proto/timestamp")
               (:file "lisp/proto/cl-proto/wrappers")
               ;; cl-protobufs
               (:file "lisp/cl-protobufs/serialize")
               (:file "lisp/cl-protobufs/well-known-types")
               (:file "lisp/cl-protobufs/message-api")
               (:file "lisp/cl-protobufs/json")))

