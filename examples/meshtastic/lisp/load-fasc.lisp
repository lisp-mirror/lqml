;;; loading byte-compiled proto files is much faster (especially on mobile)
;;; and doesn't really impact performance at runtime

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
    (load (merge-pathnames (format nil "~Alisp/proto/meshtastic/~A.fasc"
                                   (if make "examples/meshtastic/" "")
                                   file)))))

