;;; loading protobuf Lisp source files is much faster (especially on mobile)
;;; and doesn't really impact performance at runtime (in this use case)

(in-package :qml-user)

(let ((make (find :make *features*))) ; runtime check needed
  (dolist (file (list "xmodem"
                      "telemetry"
                      "portnums"
                      "module-config"
                      "config"
                      "channel"
                      "connection-status"
                      "admin"
                      "mesh"
                      "storeforward"
                      "apponly"
                      "localonly"
                      "clientonly"
                      "deviceonly"
                      "remote-hardware"
                      "cannedmessages"
                      "mqtt"
                      "rtttl"))
    (if make
        (load (format nil "examples/meshtastic/lisp/proto/meshtastic/~A.lisp" file))
        (let ((file* (format nil "lisp/proto/meshtastic/~A.lisp" file)))
          (if #+mobile nil #-mobile (probe-file file*)
              (load file*)          ; development
              (qload-rc file*)))))) ; final app

