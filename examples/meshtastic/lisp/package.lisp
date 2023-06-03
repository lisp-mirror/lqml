(defpackage :app
  (:use :cl :qml)
  (:export))

(defpackage :radio
  (:use :cl :qml)
  (:local-nicknames (:pr :cl-protobufs)
                    (:me :cl-protobufs.meshtastic))
  (:export
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
   #:start-config
   #:read-radio
   #:received-from-radio
   #:receiving-done
   #:send-message
   #:send-to-radio
   #:set-ready))

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
