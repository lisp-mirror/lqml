;;; meshtastic/apponly.proto.lisp
;;;
;;; Generated by the protocol buffer compiler. DO NOT EDIT!

(cl:in-package #:common-lisp-user)

#+sbcl
(cl:progn
 (cl:eval-when (:compile-toplevel) (sb-ext:restrict-compiler-policy 'cl:debug 0 1))
 (cl:declaim (cl:optimize (sb-c:store-coverage-data 0))))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:unless (cl:find-package "CL-PROTOBUFS.MESHTASTIC")
    (cl:defpackage "CL-PROTOBUFS.MESHTASTIC" (:use)
                   (:local-nicknames (#:pi #:cl-protobufs.implementation)))))

(cl:in-package "CL-PROTOBUFS.MESHTASTIC")

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
(pi:define-schema 'apponly
    :syntax :proto3

     :package "meshtastic"
))


;;; Top-Level messages

(pi:define-message channel-set
    ()
  ;; Fields
  (settings
   :index 1 :type cl-protobufs.meshtastic::channel-settings :kind :message :label (:repeated :list) :json-name "settings")
  (lora-config
   :index 2 :type cl-protobufs.meshtastic::config.lo-ra-config :kind :message :label (:optional) :json-name "loraConfig"))

(cl:export '(apponly
             channel-set
             lora-config
             settings))
