(defpackage :app
  (:use :cl :qml)
  (:export))

(defpackage :radio
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
   #:*region*
   #:*remote-node*
   #:channel-to-url
   #:start-config
   #:read-radio
   #:received-from-radio
   #:receiving-done
   #:send-message
   #:send-to-radio
   #:set-fixed-pin
   #:set-ready
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
