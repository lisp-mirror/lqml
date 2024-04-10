;;; generates QML file for animation

(load "../package")
(load "../definitions")
(load "../utils")
(load "with-qml-file")

(in-package :pal)

(with-qml-file ("../../qml/main.qml")
  "import QtQuick 2.15"
  "import 'ext/' as Ext"
  (qml "Rectangle"
       "width: 527; height: 527"
       "color: 'black'"
       (qml "Rectangle"
            "x: scale * (width - 527) / 2"
            "y: scale * (height - 527) / 2"
            "width: parent.width"
            "height: parent.height"
            "color: 'black'"
            "scale: Math.min(width, height) / 527"
            ""
            (let ((num 0))
              (mapc (lambda (char xy)
                      (incf num)
                      (qml "Ext.PalindromeImage { objectName: 'img~D'; source: 'img/~A.png'; x: ~D; y: ~D }"
                           num
                           (image-of-char char)
                           (* 31 (first xy))
                           (* 31 (second xy))))
                    *chars* (first *move-to-positions*))))))

(qlater 'qquit)

