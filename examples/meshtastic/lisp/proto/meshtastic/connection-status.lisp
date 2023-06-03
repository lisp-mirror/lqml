;;; meshtastic/connection_status.proto.lisp
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
(pi:define-schema 'connection_status
    :syntax :proto3

     :package "meshtastic")
)


;;; Top-Level messages

(pi:define-message device-connection-status
    ()
  ;; Fields
  (pi:define-oneof -wifi (:synthetic-p t)
    (wifi
     :index 1 :type wifi-connection-status :kind :message :label (:optional) :json-name "wifi"))
  (pi:define-oneof -ethernet (:synthetic-p t)
    (ethernet
     :index 2 :type ethernet-connection-status :kind :message :label (:optional) :json-name "ethernet"))
  (pi:define-oneof -bluetooth (:synthetic-p t)
    (bluetooth
     :index 3 :type bluetooth-connection-status :kind :message :label (:optional) :json-name "bluetooth"))
  (pi:define-oneof -serial (:synthetic-p t)
    (serial
     :index 4 :type serial-connection-status :kind :message :label (:optional) :json-name "serial")))

(pi:define-message wifi-connection-status
    ()
  ;; Fields
  (status
   :index 1 :type network-connection-status :kind :message :label (:optional) :json-name "status")
  (ssid
   :index 2 :type cl:string :kind :scalar :label (:optional) :json-name "ssid")
  (rssi
   :index 3 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "rssi"))

(pi:define-message ethernet-connection-status
    ()
  ;; Fields
  (status
   :index 1 :type network-connection-status :kind :message :label (:optional) :json-name "status"))

(pi:define-message network-connection-status
    ()
  ;; Fields
  (ip-address
   :index 1 :type cl-protobufs:fixed32 :kind :scalar :label (:optional) :json-name "ipAddress")
  (is-connected
   :index 2 :type cl:boolean :kind :scalar :label (:optional) :json-name "isConnected")
  (is-mqtt-connected
   :index 3 :type cl:boolean :kind :scalar :label (:optional) :json-name "isMqttConnected")
  (is-syslog-connected
   :index 4 :type cl:boolean :kind :scalar :label (:optional) :json-name "isSyslogConnected"))

(pi:define-message bluetooth-connection-status
    ()
  ;; Fields
  (pin
   :index 1 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "pin")
  (rssi
   :index 2 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "rssi")
  (is-connected
   :index 3 :type cl:boolean :kind :scalar :label (:optional) :json-name "isConnected"))

(pi:define-message serial-connection-status
    ()
  ;; Fields
  (baud
   :index 1 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "baud")
  (is-connected
   :index 2 :type cl:boolean :kind :scalar :label (:optional) :json-name "isConnected"))

(cl:export '(baud
             bluetooth
             bluetooth-connection-status
             connection_status
             device-connection-status
             ethernet
             ethernet-connection-status
             ip-address
             is-connected
             is-mqtt-connected
             is-syslog-connected
             network-connection-status
             pin
             rssi
             serial
             serial-connection-status
             ssid
             status
             wifi
             wifi-connection-status))
