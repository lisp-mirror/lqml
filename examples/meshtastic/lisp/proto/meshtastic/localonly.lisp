;;; meshtastic/localonly.proto.lisp
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
(pi:define-schema 'localonly
    :syntax :proto3

     :package "meshtastic"
     :import '(;;"meshtastic/config.proto"
               ;;"meshtastic/module_config.proto"
               ))
)


;;; Top-Level messages

(pi:define-message local-config
    ()
  ;; Fields
  (device
   :index 1 :type cl-protobufs.meshtastic::config.device-config :kind :message :label (:optional) :json-name "device")
  (position
   :index 2 :type cl-protobufs.meshtastic::config.position-config :kind :message :label (:optional) :json-name "position")
  (power
   :index 3 :type cl-protobufs.meshtastic::config.power-config :kind :message :label (:optional) :json-name "power")
  (network
   :index 4 :type cl-protobufs.meshtastic::config.network-config :kind :message :label (:optional) :json-name "network")
  (display
   :index 5 :type cl-protobufs.meshtastic::config.display-config :kind :message :label (:optional) :json-name "display")
  (lora
   :index 6 :type cl-protobufs.meshtastic::config.lo-ra-config :kind :message :label (:optional) :json-name "lora")
  (bluetooth
   :index 7 :type cl-protobufs.meshtastic::config.bluetooth-config :kind :message :label (:optional) :json-name "bluetooth")
  (version
   :index 8 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "version"))

(pi:define-message local-module-config
    ()
  ;; Fields
  (mqtt
   :index 1 :type cl-protobufs.meshtastic::module-config.mqtt-config :kind :message :label (:optional) :json-name "mqtt")
  (serial
   :index 2 :type cl-protobufs.meshtastic::module-config.serial-config :kind :message :label (:optional) :json-name "serial")
  (external-notification
   :index 3 :type cl-protobufs.meshtastic::module-config.external-notification-config :kind :message :label (:optional) :json-name "externalNotification")
  (store-forward
   :index 4 :type cl-protobufs.meshtastic::module-config.store-forward-config :kind :message :label (:optional) :json-name "storeForward")
  (range-test
   :index 5 :type cl-protobufs.meshtastic::module-config.range-test-config :kind :message :label (:optional) :json-name "rangeTest")
  (telemetry
   :index 6 :type cl-protobufs.meshtastic::module-config.telemetry-config :kind :message :label (:optional) :json-name "telemetry")
  (canned-message
   :index 7 :type cl-protobufs.meshtastic::module-config.canned-message-config :kind :message :label (:optional) :json-name "cannedMessage")
  (audio
   :index 9 :type cl-protobufs.meshtastic::module-config.audio-config :kind :message :label (:optional) :json-name "audio")
  (remote-hardware
   :index 10 :type cl-protobufs.meshtastic::module-config.remote-hardware-config :kind :message :label (:optional) :json-name "remoteHardware")
  (version
   :index 8 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "version"))

(cl:export '(audio
             bluetooth
             canned-message
             device
             display
             external-notification
             local-config
             local-module-config
             localonly
             lora
             mqtt
             network
             position
             power
             range-test
             remote-hardware
             serial
             store-forward
             telemetry
             version))
