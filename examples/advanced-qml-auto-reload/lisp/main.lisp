(in-package :app)

#+mobile
(qsingle-shot 1000 (lambda ()
                     (when qml::*remote-ip*
                       (qjs |message| ui:*dialogs*
                            (format nil "<qt>QML auto reload enabled from: <br><br>~A<br><br>Ensure <b>web-server.sh</b> is running.</qt>"
                                    qml::*remote-ip*))
                       (auto-reload-qml))))
