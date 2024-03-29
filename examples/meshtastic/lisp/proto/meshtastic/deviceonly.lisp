;;; meshtastic/deviceonly.proto.lisp
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
(pi:define-schema 'deviceonly
    :syntax :proto3

     :package "meshtastic"
))


;;; Top-Level enums

(pi:define-enum screen-fonts
    ()
  (:font-small :index 0)
  (:font-medium :index 1)
  (:font-large :index 2))

;;; Top-Level messages

(pi:define-message device-state
    ()
  ;; Fields
  (my-node
   :index 2 :type cl-protobufs.meshtastic::my-node-info :kind :message :label (:optional) :json-name "myNode")
  (owner
   :index 3 :type cl-protobufs.meshtastic::user :kind :message :label (:optional) :json-name "owner")
  (node-db
   :index 4 :type cl-protobufs.meshtastic::node-info :kind :message :label (:repeated :list) :json-name "nodeDb")
  (receive-queue
   :index 5 :type cl-protobufs.meshtastic::mesh-packet :kind :message :label (:repeated :list) :json-name "receiveQueue")
  (version
   :index 8 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "version")
  (rx-text-message
   :index 7 :type cl-protobufs.meshtastic::mesh-packet :kind :message :label (:optional) :json-name "rxTextMessage")
  (no-save
   :index 9 :type cl:boolean :kind :scalar :label (:optional) :json-name "noSave")
  (did-gps-reset
   :index 11 :type cl:boolean :kind :scalar :label (:optional) :json-name "didGpsReset")
  (rx-waypoint
   :index 12 :type cl-protobufs.meshtastic::mesh-packet :kind :message :label (:optional) :json-name "rxWaypoint")
  (node-remote-hardware-pins
   :index 13 :type node-remote-hardware-pin :kind :message :label (:repeated :list) :json-name "nodeRemoteHardwarePins")
  (node-db-lite
   :index 14 :type node-info-lite :kind :message :label (:repeated :list) :json-name "nodeDbLite"))

(pi:define-message node-info-lite
    ()
  ;; Fields
  (num
   :index 1 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "num")
  (user
   :index 2 :type cl-protobufs.meshtastic::user :kind :message :label (:optional) :json-name "user")
  (position
   :index 3 :type position-lite :kind :message :label (:optional) :json-name "position")
  (snr
   :index 4 :type cl:float :kind :scalar :label (:optional) :json-name "snr")
  (last-heard
   :index 5 :type cl-protobufs:fixed32 :kind :scalar :label (:optional) :json-name "lastHeard")
  (device-metrics
   :index 6 :type cl-protobufs.meshtastic::device-metrics :kind :message :label (:optional) :json-name "deviceMetrics")
  (channel
   :index 7 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "channel"))

(pi:define-message position-lite
    ()
  ;; Fields
  (latitude-i
   :index 1 :type cl-protobufs:sfixed32 :kind :scalar :label (:optional) :json-name "latitudeI")
  (longitude-i
   :index 2 :type cl-protobufs:sfixed32 :kind :scalar :label (:optional) :json-name "longitudeI")
  (altitude
   :index 3 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "altitude")
  (time
   :index 4 :type cl-protobufs:fixed32 :kind :scalar :label (:optional) :json-name "time")
  (location-source
   :index 5 :type cl-protobufs.meshtastic::position.loc-source :kind :enum :label (:optional) :json-name "locationSource" :default :loc-unset))

(pi:define-message channel-file
    ()
  ;; Fields
  (channels
   :index 1 :type cl-protobufs.meshtastic::channel :kind :message :label (:repeated :list) :json-name "channels")
  (version
   :index 2 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "version"))

(pi:define-message oem-store
    (
     :name "OEMStore")
  ;; Fields
  (oem-icon-width
   :index 1 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "oemIconWidth")
  (oem-icon-height
   :index 2 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "oemIconHeight")
  (oem-icon-bits
   :index 3 :type cl-protobufs:byte-vector :kind :scalar :label (:optional) :json-name "oemIconBits")
  (oem-font
   :index 4 :type screen-fonts :kind :enum :label (:optional) :json-name "oemFont" :default :font-small)
  (oem-text
   :index 5 :type cl:string :kind :scalar :label (:optional) :json-name "oemText")
  (oem-aes-key
   :index 6 :type cl-protobufs:byte-vector :kind :scalar :label (:optional) :json-name "oemAesKey")
  (oem-local-config
   :index 7 :type cl-protobufs.meshtastic::local-config :kind :message :label (:optional) :json-name "oemLocalConfig")
  (oem-local-module-config
   :index 8 :type cl-protobufs.meshtastic::local-module-config :kind :message :label (:optional) :json-name "oemLocalModuleConfig"))

(pi:define-message node-remote-hardware-pin
    ()
  ;; Fields
  (node-num
   :index 1 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "nodeNum")
  (pin
   :index 2 :type cl-protobufs.meshtastic::remote-hardware-pin :kind :message :label (:optional) :json-name "pin"))

(cl:export '(altitude
             channel
             channel-file
             channels
             device-metrics
             device-state
             deviceonly
             did-gps-reset
             last-heard
             latitude-i
             location-source
             longitude-i
             my-node
             no-save
             node-db
             node-db-lite
             node-info-lite
             node-num
             node-remote-hardware-pin
             node-remote-hardware-pins
             num
             oem-aes-key
             oem-font
             oem-icon-bits
             oem-icon-height
             oem-icon-width
             oem-local-config
             oem-local-module-config
             oem-store
             oem-text
             owner
             pin
             position
             position-lite
             receive-queue
             rx-text-message
             rx-waypoint
             screen-fonts
             screen-fonts-int-to-keyword
             screen-fonts-keyword-to-int
             snr
             time
             user
             version))
