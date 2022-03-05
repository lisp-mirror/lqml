(in-package :app)

#+(or android ios)
(qsingle-shot 1000 'auto-reload-qml)
