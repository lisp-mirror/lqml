;;; meshtastic/config.proto.lisp
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
(pi:define-schema 'config
    :syntax :proto3

     :package "meshtastic")
)


;;; Top-Level messages

(pi:define-message config
    ()
  ;; Nested messages

  (pi:define-message config.device-config
      ()
    ;; Nested enums

    (pi:define-enum config.device-config.role
        ()
      (:client :index 0)
      (:client-mute :index 1)
      (:router :index 2)
      (:router-client :index 3)
      (:repeater :index 4)
      (:tracker :index 5)
      (:sensor :index 6))

    (pi:define-enum config.device-config.rebroadcast-mode
        ()
      (:all :index 0)
      (:all-skip-decoding :index 1)
      (:local-only :index 2))
    ;; Fields
    (role
     :index 1 :type config.device-config.role :kind :enum :label (:optional) :json-name "role" :default :client)
    (serial-enabled
     :index 2 :type cl:boolean :kind :scalar :label (:optional) :json-name "serialEnabled")
    (debug-log-enabled
     :index 3 :type cl:boolean :kind :scalar :label (:optional) :json-name "debugLogEnabled")
    (button-gpio
     :index 4 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "buttonGpio")
    (buzzer-gpio
     :index 5 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "buzzerGpio")
    (rebroadcast-mode
     :index 6 :type config.device-config.rebroadcast-mode :kind :enum :label (:optional) :json-name "rebroadcastMode" :default :all)
    (node-info-broadcast-secs
     :index 7 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "nodeInfoBroadcastSecs")
    (double-tap-as-button-press
     :index 8 :type cl:boolean :kind :scalar :label (:optional) :json-name "doubleTapAsButtonPress"))

  (pi:define-message config.position-config
      ()
    ;; Nested enums

    (pi:define-enum config.position-config.position-flags
        ()
      (:unset :index 0)
      (:altitude :index 1)
      (:altitude-msl :index 2)
      (:geoidal-separation :index 4)
      (:dop :index 8)
      (:hvdop :index 16)
      (:satinview :index 32)
      (:seq-no :index 64)
      (:timestamp :index 128)
      (:heading :index 256)
      (:speed :index 512))
    ;; Fields
    (position-broadcast-secs
     :index 1 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "positionBroadcastSecs")
    (position-broadcast-smart-enabled
     :index 2 :type cl:boolean :kind :scalar :label (:optional) :json-name "positionBroadcastSmartEnabled")
    (fixed-position
     :index 3 :type cl:boolean :kind :scalar :label (:optional) :json-name "fixedPosition")
    (gps-enabled
     :index 4 :type cl:boolean :kind :scalar :label (:optional) :json-name "gpsEnabled")
    (gps-update-interval
     :index 5 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "gpsUpdateInterval")
    (gps-attempt-time
     :index 6 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "gpsAttemptTime")
    (position-flags
     :index 7 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "positionFlags")
    (rx-gpio
     :index 8 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "rxGpio")
    (tx-gpio
     :index 9 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "txGpio")
    (broadcast-smart-minimum-distance
     :index 10 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "broadcastSmartMinimumDistance")
    (broadcast-smart-minimum-interval-secs
     :index 11 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "broadcastSmartMinimumIntervalSecs"))

  (pi:define-message config.power-config
      ()
    ;; Fields
    (is-power-saving
     :index 1 :type cl:boolean :kind :scalar :label (:optional) :json-name "isPowerSaving")
    (on-battery-shutdown-after-secs
     :index 2 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "onBatteryShutdownAfterSecs")
    (adc-multiplier-override
     :index 3 :type cl:float :kind :scalar :label (:optional) :json-name "adcMultiplierOverride")
    (wait-bluetooth-secs
     :index 4 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "waitBluetoothSecs")
    (mesh-sds-timeout-secs
     :index 5 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "meshSdsTimeoutSecs")
    (sds-secs
     :index 6 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "sdsSecs")
    (ls-secs
     :index 7 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "lsSecs")
    (min-wake-secs
     :index 8 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "minWakeSecs"))

  (pi:define-message config.network-config
      ()
    ;; Nested enums

    (pi:define-enum config.network-config.address-mode
        ()
      (:dhcp :index 0)
      (:static :index 1))
    ;; Nested messages

    (pi:define-message config.network-config.ip-v4-config
        ()
      ;; Fields
      (ip
       :index 1 :type cl-protobufs:fixed32 :kind :scalar :label (:optional) :json-name "ip")
      (gateway
       :index 2 :type cl-protobufs:fixed32 :kind :scalar :label (:optional) :json-name "gateway")
      (subnet
       :index 3 :type cl-protobufs:fixed32 :kind :scalar :label (:optional) :json-name "subnet")
      (dns
       :index 4 :type cl-protobufs:fixed32 :kind :scalar :label (:optional) :json-name "dns"))
    ;; Fields
    (wifi-enabled
     :index 1 :type cl:boolean :kind :scalar :label (:optional) :json-name "wifiEnabled")
    (wifi-ssid
     :index 3 :type cl:string :kind :scalar :label (:optional) :json-name "wifiSsid")
    (wifi-psk
     :index 4 :type cl:string :kind :scalar :label (:optional) :json-name "wifiPsk")
    (ntp-server
     :index 5 :type cl:string :kind :scalar :label (:optional) :json-name "ntpServer")
    (eth-enabled
     :index 6 :type cl:boolean :kind :scalar :label (:optional) :json-name "ethEnabled")
    (address-mode
     :index 7 :type config.network-config.address-mode :kind :enum :label (:optional) :json-name "addressMode" :default :dhcp)
    (ipv4-config
     :index 8 :type config.network-config.ip-v4-config :kind :message :label (:optional) :json-name "ipv4Config")
    (rsyslog-server
     :index 9 :type cl:string :kind :scalar :label (:optional) :json-name "rsyslogServer"))

  (pi:define-message config.display-config
      ()
    ;; Nested enums

    (pi:define-enum config.display-config.gps-coordinate-format
        ()
      (:dec :index 0)
      (:dms :index 1)
      (:utm :index 2)
      (:mgrs :index 3)
      (:olc :index 4)
      (:osgr :index 5))

    (pi:define-enum config.display-config.display-units
        ()
      (:metric :index 0)
      (:imperial :index 1))

    (pi:define-enum config.display-config.oled-type
        ()
      (:oled-auto :index 0)
      (:oled-ssd1306 :index 1)
      (:oled-sh1106 :index 2)
      (:oled-sh1107 :index 3))

    (pi:define-enum config.display-config.display-mode
        ()
      (:default :index 0)
      (:twocolor :index 1)
      (:inverted :index 2)
      (:color :index 3))
    ;; Fields
    (screen-on-secs
     :index 1 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "screenOnSecs")
    (gps-format
     :index 2 :type config.display-config.gps-coordinate-format :kind :enum :label (:optional) :json-name "gpsFormat" :default :dec)
    (auto-screen-carousel-secs
     :index 3 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "autoScreenCarouselSecs")
    (compass-north-top
     :index 4 :type cl:boolean :kind :scalar :label (:optional) :json-name "compassNorthTop")
    (flip-screen
     :index 5 :type cl:boolean :kind :scalar :label (:optional) :json-name "flipScreen")
    (units
     :index 6 :type config.display-config.display-units :kind :enum :label (:optional) :json-name "units" :default :metric)
    (oled
     :index 7 :type config.display-config.oled-type :kind :enum :label (:optional) :json-name "oled" :default :oled-auto)
    (displaymode
     :index 8 :type config.display-config.display-mode :kind :enum :label (:optional) :json-name "displaymode" :default :default)
    (heading-bold
     :index 9 :type cl:boolean :kind :scalar :label (:optional) :json-name "headingBold")
    (wake-on-tap-or-motion
     :index 10 :type cl:boolean :kind :scalar :label (:optional) :json-name "wakeOnTapOrMotion"))

  (pi:define-message config.lo-ra-config
      ()
    ;; Nested enums

    (pi:define-enum config.lo-ra-config.region-code
        ()
      (:unset :index 0)
      (:us :index 1)
      (:eu-433 :index 2)
      (:eu-868 :index 3)
      (:cn :index 4)
      (:jp :index 5)
      (:anz :index 6)
      (:kr :index 7)
      (:tw :index 8)
      (:ru :index 9)
      (:in :index 10)
      (:nz-865 :index 11)
      (:th :index 12)
      (:lora-24 :index 13)
      (:ua-433 :index 14)
      (:ua-868 :index 15))

    (pi:define-enum config.lo-ra-config.modem-preset
        ()
      (:long-fast :index 0)
      (:long-slow :index 1)
      (:very-long-slow :index 2)
      (:medium-slow :index 3)
      (:medium-fast :index 4)
      (:short-slow :index 5)
      (:short-fast :index 6)
      (:long-moderate :index 7))
    ;; Fields
    (use-preset
     :index 1 :type cl:boolean :kind :scalar :label (:optional) :json-name "usePreset")
    (modem-preset
     :index 2 :type config.lo-ra-config.modem-preset :kind :enum :label (:optional) :json-name "modemPreset" :default :long-fast)
    (bandwidth
     :index 3 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "bandwidth")
    (spread-factor
     :index 4 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "spreadFactor")
    (coding-rate
     :index 5 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "codingRate")
    (frequency-offset
     :index 6 :type cl:float :kind :scalar :label (:optional) :json-name "frequencyOffset")
    (region
     :index 7 :type config.lo-ra-config.region-code :kind :enum :label (:optional) :json-name "region" :default :unset)
    (hop-limit
     :index 8 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "hopLimit")
    (tx-enabled
     :index 9 :type cl:boolean :kind :scalar :label (:optional) :json-name "txEnabled")
    (tx-power
     :index 10 :type cl-protobufs:int32 :kind :scalar :label (:optional) :json-name "txPower")
    (channel-num
     :index 11 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "channelNum")
    (override-duty-cycle
     :index 12 :type cl:boolean :kind :scalar :label (:optional) :json-name "overrideDutyCycle")
    (sx126x-rx-boosted-gain
     :index 13 :type cl:boolean :kind :scalar :label (:optional) :json-name "sx126xRxBoostedGain")
    (override-frequency
     :index 14 :type cl:float :kind :scalar :label (:optional) :json-name "overrideFrequency")
    (ignore-incoming
     :index 103 :type cl-protobufs:uint32 :kind :scalar :label (:repeated :list) :json-name "ignoreIncoming"))

  (pi:define-message config.bluetooth-config
      ()
    ;; Nested enums

    (pi:define-enum config.bluetooth-config.pairing-mode
        ()
      (:random-pin :index 0)
      (:fixed-pin :index 1)
      (:no-pin :index 2))
    ;; Fields
    (enabled
     :index 1 :type cl:boolean :kind :scalar :label (:optional) :json-name "enabled")
    (mode
     :index 2 :type config.bluetooth-config.pairing-mode :kind :enum :label (:optional) :json-name "mode" :default :random-pin)
    (fixed-pin
     :index 3 :type cl-protobufs:uint32 :kind :scalar :label (:optional) :json-name "fixedPin"))
  ;; Fields
  (pi:define-oneof payload-variant ()
    (device
     :index 1 :type config.device-config :kind :message :label (:optional) :json-name "device")
    (position
     :index 2 :type config.position-config :kind :message :label (:optional) :json-name "position")
    (power
     :index 3 :type config.power-config :kind :message :label (:optional) :json-name "power")
    (network
     :index 4 :type config.network-config :kind :message :label (:optional) :json-name "network")
    (display
     :index 5 :type config.display-config :kind :message :label (:optional) :json-name "display")
    (lora
     :index 6 :type config.lo-ra-config :kind :message :label (:optional) :json-name "lora")
    (bluetooth
     :index 7 :type config.bluetooth-config :kind :message :label (:optional) :json-name "bluetooth")))

(cl:export '(adc-multiplier-override
             address-mode
             auto-screen-carousel-secs
             bandwidth
             bluetooth
             broadcast-smart-minimum-distance
             broadcast-smart-minimum-interval-secs
             button-gpio
             buzzer-gpio
             channel-num
             coding-rate
             compass-north-top
             config
             config.bluetooth-config
             config.bluetooth-config.pairing-mode
             config.bluetooth-config.pairing-mode-int-to-keyword
             config.bluetooth-config.pairing-mode-keyword-to-int
             config.device-config
             config.device-config.rebroadcast-mode
             config.device-config.rebroadcast-mode-int-to-keyword
             config.device-config.rebroadcast-mode-keyword-to-int
             config.device-config.role
             config.device-config.role-int-to-keyword
             config.device-config.role-keyword-to-int
             config.display-config
             config.display-config.display-mode
             config.display-config.display-mode-int-to-keyword
             config.display-config.display-mode-keyword-to-int
             config.display-config.display-units
             config.display-config.display-units-int-to-keyword
             config.display-config.display-units-keyword-to-int
             config.display-config.gps-coordinate-format
             config.display-config.gps-coordinate-format-int-to-keyword
             config.display-config.gps-coordinate-format-keyword-to-int
             config.display-config.oled-type
             config.display-config.oled-type-int-to-keyword
             config.display-config.oled-type-keyword-to-int
             config.lo-ra-config
             config.lo-ra-config.modem-preset
             config.lo-ra-config.modem-preset-int-to-keyword
             config.lo-ra-config.modem-preset-keyword-to-int
             config.lo-ra-config.region-code
             config.lo-ra-config.region-code-int-to-keyword
             config.lo-ra-config.region-code-keyword-to-int
             config.network-config
             config.network-config.address-mode
             config.network-config.address-mode-int-to-keyword
             config.network-config.address-mode-keyword-to-int
             config.network-config.ip-v4-config
             config.position-config
             config.position-config.position-flags
             config.position-config.position-flags-int-to-keyword
             config.position-config.position-flags-keyword-to-int
             config.power-config
             debug-log-enabled
             device
             display
             displaymode
             dns
             double-tap-as-button-press
             enabled
             eth-enabled
             fixed-pin
             fixed-position
             flip-screen
             frequency-offset
             gateway
             gps-attempt-time
             gps-enabled
             gps-format
             gps-update-interval
             heading-bold
             hop-limit
             ignore-incoming
             ip
             ipv4-config
             is-power-saving
             lora
             ls-secs
             mesh-sds-timeout-secs
             min-wake-secs
             mode
             modem-preset
             network
             node-info-broadcast-secs
             ntp-server
             oled
             on-battery-shutdown-after-secs
             override-duty-cycle
             override-frequency
             position
             position-broadcast-secs
             position-broadcast-smart-enabled
             position-flags
             power
             rebroadcast-mode
             region
             role
             rsyslog-server
             rx-gpio
             screen-on-secs
             sds-secs
             serial-enabled
             spread-factor
             subnet
             sx126x-rx-boosted-gain
             tx-enabled
             tx-gpio
             tx-power
             units
             use-preset
             wait-bluetooth-secs
             wake-on-tap-or-motion
             wifi-enabled
             wifi-psk
             wifi-ssid))
