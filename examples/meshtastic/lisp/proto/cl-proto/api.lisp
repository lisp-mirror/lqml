;;; api.proto.lisp
;;;
;;; Generated by the protocol buffer compiler. DO NOT EDIT!

(cl:in-package #:common-lisp-user)

#+sbcl
(cl:progn
 (cl:eval-when (:compile-toplevel) (sb-ext:restrict-compiler-policy 'cl:debug 0 1))
 (cl:declaim (cl:optimize (sb-c:store-coverage-data 0))))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:unless (cl:find-package "CL-PROTOBUFS.GOOGLE.PROTOBUF")
    (cl:defpackage "CL-PROTOBUFS.GOOGLE.PROTOBUF" (:use)
                   (:local-nicknames (#:pi #:cl-protobufs.implementation)))))

(cl:in-package "CL-PROTOBUFS.GOOGLE.PROTOBUF")

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
(pi:define-schema 'api
    :syntax :proto3

     :package "google.protobuf"))

;;; Top-Level messages

(pi:define-message api
    ()
  ;; Fields
  (name
   :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "name")
  (methods
   :index 2 :type method :kind :message :label (:repeated :list) :json-name "methods")
  (options
   :index 3 :type cl-protobufs.google.protobuf::option :kind :message :label (:repeated :list) :json-name "options")
  (version
   :index 4 :type cl:string :kind :scalar :label (:optional) :json-name "version")
  (source-context
   :index 5 :type cl-protobufs.google.protobuf::source-context :kind :message :label (:optional) :json-name "sourceContext")
  (mixins
   :index 6 :type mixin :kind :message :label (:repeated :list) :json-name "mixins")
  (syntax
   :index 7 :type cl-protobufs.google.protobuf::syntax :kind :enum :label (:optional) :json-name "syntax" :default :syntax-proto2))

(pi:define-message method
    ()
  ;; Fields
  (name
   :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "name")
  (request-type-url
   :index 2 :type cl:string :kind :scalar :label (:optional) :json-name "requestTypeUrl")
  (request-streaming
   :index 3 :type cl:boolean :kind :scalar :label (:optional) :json-name "requestStreaming")
  (response-type-url
   :index 4 :type cl:string :kind :scalar :label (:optional) :json-name "responseTypeUrl")
  (response-streaming
   :index 5 :type cl:boolean :kind :scalar :label (:optional) :json-name "responseStreaming")
  (options
   :index 6 :type cl-protobufs.google.protobuf::option :kind :message :label (:repeated :list) :json-name "options")
  (syntax
   :index 7 :type cl-protobufs.google.protobuf::syntax :kind :enum :label (:optional) :json-name "syntax" :default :syntax-proto2))

(pi:define-message mixin
    ()
  ;; Fields
  (name
   :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "name")
  (root
   :index 2 :type cl:string :kind :scalar :label (:optional) :json-name "root"))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
(pi:add-file-descriptor #P"api.proto" 'api)
)

(cl:export '(api
             method
             methods
             mixin
             mixins
             name
             options
             request-streaming
             request-type-url
             response-streaming
             response-type-url
             root
             source-context
             syntax
             version))
