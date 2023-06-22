(defpackage :app
  (:use :cl :qml)
  (:export
   #:load-settings
   #:save-settings))

(defpackage :lora
  (:use :cl :qml)
  (:local-nicknames (:pr :cl-protobufs)
                    (:me :cl-protobufs.meshtastic))
  (:export
   #:*channel*
   #:*channels*
   #:*config-lora*
   #:*my-node-info*
   #:*node-infos*
   #:*primary-channel*
   #:*reading*
   #:*ready*
   #:*received*
   #:*remote-node*
   #:*settings*
   #:channel-to-url
   #:start-config
   #:start-device-discovery
   #:read-radio
   #:received-from-radio
   #:send-message
   #:send-to-radio
   #:url-to-channel))

(defpackage :messages
  (:nicknames :msg)
  (:use :cl :qml)
  (:export
   #:*messages*
   #:*message-id*
   #:*states*
   #:add-message
   #:change-state
   #:load-messages
   #:save-messages))

(defpackage :radios
  (:use :cl :qml)
  (:export
  #:*schedule-clear*
  #:add-radio
  #:change-radio
  #:clear))

