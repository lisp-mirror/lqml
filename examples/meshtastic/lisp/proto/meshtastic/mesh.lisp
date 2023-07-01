;;; meshtastic/mesh.proto.lisp
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
(pi:define-schema 'mesh
    :syntax :proto3

     :package "meshtastic"
     :import '(;;"meshtastic/channel.proto"
               ;;"meshtastic/config.proto"
               ;;"meshtastic/module_config.proto"
               ;;"meshtastic/portnums.proto"
               ;;"meshtastic/telemetry.proto"
               ;;"meshtastic/xmodem.proto"
               ))
)


;;; Top-Level enums

(pi:define-enum hardware-model
    ()
  (:unset :index 0)
  (:tlora-v2 :index 1)
  (:tlora-v1 :index 2)
  (:tlora-v2-1-1p6 :index 3)
  (:tbeam :index 4)
  (:heltec-v2-0 :index 5)
  (:tbeam-v0p7 :index 6)
  (:t-echo :index 7)
  (:tlora-v1-1p3 :index 8)
  (:rak4631 :index 9)
  (:heltec-v2-1 :index 10)
  (:heltec-v1 :index 11)
  (:lilygo-tbeam-s3-core :index 12)
  (:rak11200 :index 13)
  (:nano-g1 :index 14)
  (:tlora-v2-1-1p8 :index 15)
  (:tlora-t3-s3 :index 16)
  (:nano-g1-explorer :index 17)
  (:station-g1 :index 25)
  (:lora-relay-v1 :index 32)
  (:nrf52840dk :index 33)
  (:ppr :index 34)
  (:genieblocks :index 35)
  (:nrf52-unknown :index 36)
  (:portduino :index 37)
  (:android-sim :index 38)
  (:diy-v1 :index 39)
  (:nrf52840-pca10059 :index 40)
  (:dr-dev :index 41)
  (:m5stack :index 42)
  (:heltec-v3 :index 43)
  (:heltec-wsl-v3 :index 44)
  (:betafpv-2400-tx :index 45)
  (:betafpv-900-nano-tx :index 46)
  (:private-hw :index 255))

(pi:define-enum constants
    ()
  (:zero :index 0)
  (:data-payload-len :index 237))

(pi:define-enum critical-error-code
    ()
  (:none :index 0)
  (:tx-watchdog :index 1)
  (:sleep-enter-wait :index 2)
  (:no-radio :index 3)
  (:unspecified :index 4)
  (:ublox-unit-failed :index 5)
  (:no-axp192 :index 6)
  (:invalid-radio-setting :index 7)
  (:transmit-failed :index 8)
  (:brownout :index 9)
  (:sx1262-failure :index 10)
  (:radio-spi-bug :index 11))

;;; Top-Level messages

(pi:define-message position
    ()
  ;; Nested enums

  (pi:define-enum position.loc-source
      ()
    (:loc-unset :index 0)
    (:loc-manual :index 1)
    (:loc-internal :index 2)
    (:loc-external :index 3))

  (pi:define-enum position.alt-source
      ()
    (:alt-unset :index 0)
    (:alt-manual :index 1)
    (:alt-internal :index 2)
    (:alt-external :index 3)
    (:alt-barometric :index 4))
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
   :index 5 :type position.loc-source :kind :enum :label (:optional) :json-name "locationSource" :default :loc-unset)
  (altitude-source
   :index 6 :type position.alt-source :kind :enum :label (:optional) :json-name "altitudeSource" :default :alt-unset)
  (timestamp
   :index 7 :type cl-protobufs:fixed32 :kind :scalar :label (:optional) :json-name "timestamp")
  (timestamp-millis-adjust
   :index 8 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "timestampMillisAdjust")
  (altitude-hae
   :index 9 :type cl-protobufs:sint32 :kind :scalar :label (:optional) :json-name "altitudeHae")
  (altitude-geoidal-separation
   :index 10 :type cl-protobufs:sint32 :kind :scalar :label (:optional) :json-name "altitudeGeoidalSeparation")
  (pdop
   :index 11 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "PDOP")
  (hdop
   :index 12 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "HDOP")
  (vdop
   :index 13 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "VDOP")
  (gps-accuracy
   :index 14 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "gpsAccuracy")
  (ground-speed
   :index 15 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "groundSpeed")
  (ground-track
   :index 16 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "groundTrack")
  (fix-quality
   :index 17 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "fixQuality")
  (fix-type
   :index 18 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "fixType")
  (sats-in-view
   :index 19 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "satsInView")
  (sensor-id
   :index 20 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "sensorId")
  (next-update
   :index 21 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "nextUpdate")
  (seq-number
   :index 22 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "seqNumber"))

(pi:define-message user
    ()
  ;; Fields
  (id
   :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "id")
  (long-name
   :index 2 :type cl:string :kind :scalar :label (:optional) :json-name "longName")
  (short-name
   :index 3 :type cl:string :kind :scalar :label (:optional) :json-name "shortName")
  (macaddr
   :index 4 :type cl-protobufs:byte-vector :kind :scalar :label (:optional) :json-name "macaddr")
  (hw-model
   :index 5 :type hardware-model :kind :enum :label (:optional) :json-name "hwModel" :default :unset)
  (is-licensed
   :index 6 :type cl:boolean :kind :scalar :label (:optional) :json-name "isLicensed"))

(pi:define-message route-discovery
    ()
  ;; Fields
  (route
   :index 1 :type cl-protobufs:fixed32 :kind :scalar :label (:repeated :list) :json-name "route"))

(pi:define-message routing
    ()
  ;; Nested enums

  (pi:define-enum routing.error
      ()
    (:none :index 0)
    (:no-route :index 1)
    (:got-nak :index 2)
    (:timeout :index 3)
    (:no-interface :index 4)
    (:max-retransmit :index 5)
    (:no-channel :index 6)
    (:too-large :index 7)
    (:no-response :index 8)
    (:duty-cycle-limit :index 9)
    (:bad-request :index 32)
    (:not-authorized :index 33))
  ;; Fields
  (pi:define-oneof variant ()
    (route-request
     :index 1 :type route-discovery :kind :message :label (:optional) :json-name "routeRequest")
    (route-reply
     :index 2 :type route-discovery :kind :message :label (:optional) :json-name "routeReply")
    (error-reason
     :index 3 :type routing.error :kind :enum :label (:optional) :json-name "errorReason" :default :none)))

(pi:define-message data
    ()
  ;; Fields
  (portnum
   :index 1 :type cl-protobufs.meshtastic::port-num :kind :enum :label (:optional) :json-name "portnum" :default :unknown-app)
  (payload
   :index 2 :type cl-protobufs:byte-vector :kind :scalar :label (:optional) :json-name "payload")
  (want-response
   :index 3 :type cl:boolean :kind :scalar :label (:optional) :json-name "wantResponse")
  (dest
   :index 4 :type cl-protobufs:fixed32 :kind :scalar :label (:optional) :json-name "dest")
  (source
   :index 5 :type cl-protobufs:fixed32 :kind :scalar :label (:optional) :json-name "source")
  (request-id
   :index 6 :type cl-protobufs:fixed32 :kind :scalar :label (:optional) :json-name "requestId")
  (reply-id
   :index 7 :type cl-protobufs:fixed32 :kind :scalar :label (:optional) :json-name "replyId")
  (emoji
   :index 8 :type cl-protobufs:fixed32 :kind :scalar :label (:optional) :json-name "emoji"))

(pi:define-message waypoint
    ()
  ;; Fields
  (id
   :index 1 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "id")
  (latitude-i
   :index 2 :type cl-protobufs:sfixed32 :kind :scalar :label (:optional) :json-name "latitudeI")
  (longitude-i
   :index 3 :type cl-protobufs:sfixed32 :kind :scalar :label (:optional) :json-name "longitudeI")
  (expire
   :index 4 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "expire")
  (locked-to
   :index 5 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "lockedTo")
  (name
   :index 6 :type cl:string :kind :scalar :label (:optional) :json-name "name")
  (description
   :index 7 :type cl:string :kind :scalar :label (:optional) :json-name "description")
  (icon
   :index 8 :type cl-protobufs:fixed32 :kind :scalar :label (:optional) :json-name "icon"))

(pi:define-message mesh-packet
    ()
  ;; Nested enums

  (pi:define-enum mesh-packet.priority
      ()
    (:unset :index 0)
    (:min :index 1)
    (:background :index 10)
    (:default :index 64)
    (:reliable :index 70)
    (:ack :index 120)
    (:max :index 127))

  (pi:define-enum mesh-packet.delayed
      ()
    (:no-delay :index 0)
    (:delayed-broadcast :index 1)
    (:delayed-direct :index 2))
  ;; Fields
  (pi:define-oneof payload-variant ()
    (decoded
     :index 4 :type data :kind :message :label (:optional) :json-name "decoded")
    (encrypted
     :index 5 :type cl-protobufs:byte-vector :kind :scalar :label (:optional) :json-name "encrypted"))
  (from
   :index 1 :type cl-protobufs:fixed32 :kind :scalar :label (:optional) :json-name "from")
  (to
   :index 2 :type cl-protobufs:fixed32 :kind :scalar :label (:optional) :json-name "to")
  (channel
   :index 3 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "channel")
  (id
   :index 6 :type cl-protobufs:fixed32 :kind :scalar :label (:optional) :json-name "id")
  (rx-time
   :index 7 :type cl-protobufs:fixed32 :kind :scalar :label (:optional) :json-name "rxTime")
  (rx-snr
   :index 8 :type cl:float :kind :scalar :label (:optional) :json-name "rxSnr")
  (hop-limit
   :index 9 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "hopLimit")
  (want-ack
   :index 10 :type cl:boolean :kind :scalar :label (:optional) :json-name "wantAck")
  (priority
   :index 11 :type mesh-packet.priority :kind :enum :label (:optional) :json-name "priority" :default :unset)
  (rx-rssi
   :index 12 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "rxRssi")
  (delayed
   :index 13 :type mesh-packet.delayed :kind :enum :label (:optional) :json-name "delayed" :default :no-delay))

(pi:define-message node-info
    ()
  ;; Fields
  (num
   :index 1 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "num")
  (user
   :index 2 :type user :kind :message :label (:optional) :json-name "user")
  (position
   :index 3 :type position :kind :message :label (:optional) :json-name "position")
  (snr
   :index 4 :type cl:float :kind :scalar :label (:optional) :json-name "snr")
  (last-heard
   :index 5 :type cl-protobufs:fixed32 :kind :scalar :label (:optional) :json-name "lastHeard")
  (device-metrics
   :index 6 :type cl-protobufs.meshtastic::device-metrics :kind :message :label (:optional) :json-name "deviceMetrics")
  (channel
   :index 7 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "channel"))

(pi:define-message my-node-info
    ()
  ;; Fields
  (my-node-num
   :index 1 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "myNodeNum")
  (has-gps
   :index 2 :type cl:boolean :kind :scalar :label (:optional) :json-name "hasGps")
  (max-channels
   :index 3 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "maxChannels")
  (firmware-version
   :index 4 :type cl:string :kind :scalar :label (:optional) :json-name "firmwareVersion")
  (error-code
   :index 5 :type critical-error-code :kind :enum :label (:optional) :json-name "errorCode" :default :none)
  (error-address
   :index 6 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "errorAddress")
  (error-count
   :index 7 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "errorCount")
  (reboot-count
   :index 8 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "rebootCount")
  (bitrate
   :index 9 :type cl:float :kind :scalar :label (:optional) :json-name "bitrate")
  (message-timeout-msec
   :index 10 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "messageTimeoutMsec")
  (min-app-version
   :index 11 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "minAppVersion")
  (air-period-tx
   :index 12 :type cl-protobufs:uint32 :kind :scalar :label (:repeated :list) :json-name "airPeriodTx")
  (air-period-rx
   :index 13 :type cl-protobufs:uint32 :kind :scalar :label (:repeated :list) :json-name "airPeriodRx")
  (has-wifi
   :index 14 :type cl:boolean :kind :scalar :label (:optional) :json-name "hasWifi")
  (channel-utilization
   :index 15 :type cl:float :kind :scalar :label (:optional) :json-name "channelUtilization")
  (air-util-tx
   :index 16 :type cl:float :kind :scalar :label (:optional) :json-name "airUtilTx"))

(pi:define-message log-record
    ()
  ;; Nested enums

  (pi:define-enum log-record.level
      ()
    (:unset :index 0)
    (:critical :index 50)
    (:error :index 40)
    (:warning :index 30)
    (:info :index 20)
    (:debug :index 10)
    (:trace :index 5))
  ;; Fields
  (message
   :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "message")
  (time
   :index 2 :type cl-protobufs:fixed32 :kind :scalar :label (:optional) :json-name "time")
  (source
   :index 3 :type cl:string :kind :scalar :label (:optional) :json-name "source")
  (level
   :index 4 :type log-record.level :kind :enum :label (:optional) :json-name "level" :default :unset))

(pi:define-message queue-status
    ()
  ;; Fields
  (res
   :index 1 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "res")
  (free
   :index 2 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "free")
  (maxlen
   :index 3 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "maxlen")
  (mesh-packet-id
   :index 4 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "meshPacketId"))

(pi:define-message from-radio
    ()
  ;; Fields
  (pi:define-oneof payload-variant ()
    (packet
     :index 2 :type mesh-packet :kind :message :label (:optional) :json-name "packet")
    (my-info
     :index 3 :type my-node-info :kind :message :label (:optional) :json-name "myInfo")
    (node-info
     :index 4 :type node-info :kind :message :label (:optional) :json-name "nodeInfo")
    (config
     :index 5 :type cl-protobufs.meshtastic::config :kind :message :label (:optional) :json-name "config")
    (log-record
     :index 6 :type log-record :kind :message :label (:optional) :json-name "logRecord")
    (config-complete-id
     :index 7 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "configCompleteId")
    (rebooted
     :index 8 :type cl:boolean :kind :scalar :label (:optional) :json-name "rebooted")
    (module-config
     :index 9 :type cl-protobufs.meshtastic::module-config :kind :message :label (:optional) :json-name "moduleConfig")
    (channel
     :index 10 :type cl-protobufs.meshtastic::channel :kind :message :label (:optional) :json-name "channel")
    (queue-status
     :index 11 :type queue-status :kind :message :label (:optional) :json-name "queueStatus")
    (xmodem-packet
     :index 12 :type cl-protobufs.meshtastic::x-modem :kind :message :label (:optional) :json-name "xmodemPacket")
    (metadata
     :index 13 :type device-metadata :kind :message :label (:optional) :json-name "metadata"))
  (id
   :index 1 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "id"))

(pi:define-message to-radio
    ()
  ;; Fields
  (pi:define-oneof payload-variant ()
    (packet
     :index 1 :type mesh-packet :kind :message :label (:optional) :json-name "packet")
    (want-config-id
     :index 3 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "wantConfigId")
    (disconnect
     :index 4 :type cl:boolean :kind :scalar :label (:optional) :json-name "disconnect")
    (xmodem-packet
     :index 5 :type cl-protobufs.meshtastic::x-modem :kind :message :label (:optional) :json-name "xmodemPacket")))

(pi:define-message compressed
    ()
  ;; Fields
  (portnum
   :index 1 :type cl-protobufs.meshtastic::port-num :kind :enum :label (:optional) :json-name "portnum" :default :unknown-app)
  (data
   :index 2 :type cl-protobufs:byte-vector :kind :scalar :label (:optional) :json-name "data"))

(pi:define-message neighbor-info
    ()
  ;; Fields
  (node-id
   :index 1 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "nodeId")
  (last-sent-by-id
   :index 2 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "lastSentById")
  (neighbors
   :index 3 :type neighbor :kind :message :label (:repeated :list) :json-name "neighbors"))

(pi:define-message neighbor
    ()
  ;; Fields
  (node-id
   :index 1 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "nodeId")
  (snr
   :index 2 :type cl:float :kind :scalar :label (:optional) :json-name "snr"))

(pi:define-message device-metadata
    ()
  ;; Fields
  (firmware-version
   :index 1 :type cl:string :kind :scalar :label (:optional) :json-name "firmwareVersion")
  (device-state-version
   :index 2 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "deviceStateVersion")
  (can-shutdown
   :index 3 :type cl:boolean :kind :scalar :label (:optional) :json-name "canShutdown")
  (has-wifi
   :index 4 :type cl:boolean :kind :scalar :label (:optional) :json-name "hasWifi")
  (has-bluetooth
   :index 5 :type cl:boolean :kind :scalar :label (:optional) :json-name "hasBluetooth")
  (has-ethernet
   :index 6 :type cl:boolean :kind :scalar :label (:optional) :json-name "hasEthernet")
  (role
   :index 7 :type cl-protobufs.meshtastic::config.device-config.role :kind :enum :label (:optional) :json-name "role" :default :client)
  (position-flags
   :index 8 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "positionFlags")
  (hw-model
   :index 9 :type hardware-model :kind :enum :label (:optional) :json-name "hwModel" :default :unset))

(cl:export '(air-period-rx
             air-period-tx
             air-util-tx
             altitude
             altitude-geoidal-separation
             altitude-hae
             altitude-source
             bitrate
             can-shutdown
             channel
             channel-utilization
             compressed
             config
             config-complete-id
             constants
             constants-int-to-keyword
             constants-keyword-to-int
             critical-error-code
             critical-error-code-int-to-keyword
             critical-error-code-keyword-to-int
             data
             decoded
             delayed
             description
             dest
             device-metadata
             device-metrics
             device-state-version
             disconnect
             emoji
             encrypted
             error-address
             error-code
             error-count
             error-reason
             expire
             firmware-version
             fix-quality
             fix-type
             free
             from
             from-radio
             gps-accuracy
             ground-speed
             ground-track
             hardware-model
             hardware-model-int-to-keyword
             hardware-model-keyword-to-int
             has-bluetooth
             has-ethernet
             has-gps
             has-wifi
             hdop
             hop-limit
             hw-model
             icon
             id
             is-licensed
             last-heard
             last-sent-by-id
             latitude-i
             level
             location-source
             locked-to
             log-record
             log-record.level
             log-record.level-int-to-keyword
             log-record.level-keyword-to-int
             long-name
             longitude-i
             macaddr
             max-channels
             maxlen
             mesh
             mesh-packet
             mesh-packet-id
             mesh-packet.delayed
             mesh-packet.delayed-int-to-keyword
             mesh-packet.delayed-keyword-to-int
             mesh-packet.priority
             mesh-packet.priority-int-to-keyword
             mesh-packet.priority-keyword-to-int
             message
             message-timeout-msec
             metadata
             min-app-version
             module-config
             my-info
             my-node-info
             my-node-num
             name
             neighbor
             neighbor-info
             neighbors
             next-update
             node-id
             node-info
             num
             packet
             payload
             pdop
             portnum
             position
             position-flags
             position.alt-source
             position.alt-source-int-to-keyword
             position.alt-source-keyword-to-int
             position.loc-source
             position.loc-source-int-to-keyword
             position.loc-source-keyword-to-int
             priority
             queue-status
             reboot-count
             rebooted
             reply-id
             request-id
             res
             role
             route
             route-discovery
             route-reply
             route-request
             routing
             routing.error
             routing.error-int-to-keyword
             routing.error-keyword-to-int
             rx-rssi
             rx-snr
             rx-time
             sats-in-view
             sensor-id
             seq-number
             short-name
             snr
             source
             time
             timestamp
             timestamp-millis-adjust
             to
             to-radio
             user
             vdop
             want-ack
             want-config-id
             want-response
             waypoint
             xmodem-packet))