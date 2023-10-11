;;; download (using wget) all rpms for qt5.15 app development
;;;
;;; note that some files in the list have 2 versions, depending on the arch
;;; (you will see 'not found' for those files, which is not an error)

;; set arch here
(defvar *arch* (nth 0 '("armv7hl" "aarch64")))

(defvar *url* "https://repo.sailfishos.org/obs/sailfishos:/chum/4.5.0.19_~A/~A/")

(defvar *files*
 '("opt-kf5-karchive-5.108.0-1.5.2.jolla.~A.rpm"
   "opt-kf5-kauth-5.108.0-1.4.1.jolla.~A.rpm"
   "opt-kf5-kbookmarks-5.108.0-1.4.1.jolla.~A.rpm"
   "opt-kf5-kcodecs-5.108.0-1.5.1.jolla.~A.rpm"
   "opt-kf5-kcompletion-5.108.0-1.4.1.jolla.~A.rpm"
   "opt-kf5-kconfig-5.108.0-1.5.1.jolla.~A.rpm"
   "opt-kf5-kconfig-core-5.108.0-1.5.1.jolla.~A.rpm"
   "opt-kf5-kconfig-gui-5.108.0-1.5.1.jolla.~A.rpm"
   "opt-kf5-kconfigwidgets-5.108.0-1.5.1.jolla.~A.rpm"
   "opt-kf5-kcoreaddons-5.108.0-1.5.1.jolla.~A.rpm"
   "opt-kf5-kcrash-5.108.0-1.4.1.jolla.~A.rpm"
   "opt-kf5-kdbusaddons-5.108.0-1.5.1.jolla.~A.rpm"
   "opt-kf5-kguiaddons-5.108.0-1.5.1.jolla.~A.rpm"
   "opt-kf5-ki18n-5.108.0-1.5.1.jolla.~A.rpm"
   "opt-kf5-kiconthemes-5.108.0-1.5.1.jolla.~A.rpm"
   "opt-kf5-kio-core-5.108.0-1.7.1.jolla.~A.rpm"
   "opt-kf5-kio-core-libs-5.108.0-1.7.1.jolla.~A.rpm"
   "opt-kf5-kio-file-widgets-5.108.0-1.7.1.jolla.~A.rpm"
   "opt-kf5-kio-gui-5.108.0-1.7.1.jolla.~A.rpm"
   "opt-kf5-kio-widgets-5.108.0-1.7.1.jolla.~A.rpm"
   "opt-kf5-kio-widgets-libs-5.108.0-1.7.1.jolla.~A.rpm"
   "opt-kf5-kirigami2-5.108.0-1.6.1.jolla.~A.rpm"
   "opt-kf5-kitemviews-5.108.0-1.5.1.jolla.~A.rpm"
   "opt-kf5-kjobwidgets-5.108.0-1.4.1.jolla.~A.rpm"
   "opt-kf5-knotifications-5.108.0-1.5.1.jolla.~A.rpm"
   "opt-kf5-kquickcharts-5.108.0-1.5.1.jolla.~A.rpm"
   "opt-kf5-kservice-5.108.0-1.4.1.jolla.~A.rpm"
   "opt-kf5-kwallet-5.108.0-1.4.1.jolla.~A.rpm"
   "opt-kf5-kwallet-libs-5.108.0-1.4.1.jolla.~A.rpm"
   "opt-kf5-kwidgetsaddons-5.108.0-1.5.1.jolla.~A.rpm"
   "opt-kf5-kwindowsystem-5.108.0-1.5.1.jolla.~A.rpm"
   "opt-kf5-kxmlgui-5.108.0-1.4.1.jolla.~A.rpm"
   "opt-kf5-solid-5.108.0-1.4.1.jolla.~A.rpm"
   "opt-plasma-integration-5.27.4+git1-1.2.7.jolla.~A.rpm" ; armv7hl
   "opt-plasma-integration-5.27.4+git1-1.2.6.jolla.~A.rpm" ; aarch64
   "opt-qt5-qtbase-5.15.10+kde137-1.3.1.jolla.~A.rpm"
   "opt-qt5-qtbase-common-5.15.10+kde137-1.3.1.jolla.noarch.rpm"
   "opt-qt5-qtbase-devel-5.15.10+kde137-1.3.1.jolla.~A.rpm"
   "opt-qt5-qtbase-gui-5.15.10+kde137-1.3.1.jolla.~A.rpm"
   "opt-qt5-qtconnectivity-5.15.10+kde4-1.3.1.jolla.~A.rpm"
   "opt-qt5-qtconnectivity-devel-5.15.10+kde4-1.3.1.jolla.~A.rpm" 
   "opt-qt5-qtdeclarative-5.15.10+kde29-1.3.1.jolla.~A.rpm"
   "opt-qt5-qtdeclarative-devel-5.15.10+kde29-1.3.1.jolla.~A.rpm"
   "opt-qt5-qtdeclarative-tools-5.15.10+kde29-1.3.1.jolla.~A.rpm"
   "opt-qt5-qtgraphicaleffects-5.15.10-1.3.1.jolla.~A.rpm"
   "opt-qt5-qtimageformats-5.15.10+kde9-1.3.1.jolla.~A.rpm"
   "opt-qt5-qtlocation-5.15.10+kde4-1.5.1.jolla.~A.rpm"
   "opt-qt5-qtlocation-devel-5.15.10+kde4-1.5.1.jolla.~A.rpm"
   "opt-qt5-qtmultimedia-5.15.10+kde3-1.3.1.jolla.~A.rpm"
   "opt-qt5-qtmultimedia-devel-5.15.10+kde3-1.3.1.jolla.~A.rpm"
   "opt-qt5-qtquickcontrols-5.15.10-1.3.1.jolla.~A.rpm"
   "opt-qt5-qtquickcontrols2-5.15.10+kde6-1.3.1.jolla.~A.rpm"
   "opt-qt5-qtquickcontrols2-devel-5.15.10+kde6-1.3.1.jolla.~A.rpm"
   "opt-qt5-qtsensors-5.15.10-1.3.1.jolla.~A.rpm"
   "opt-qt5-qtsensors-devel-5.15.10-1.3.1.jolla.~A.rpm"
   "opt-qt5-qtsvg-5.15.10+kde8-1.3.1.jolla.~A.rpm"
   "opt-qt5-qtwayland-5.15.10+kde52-1.3.1.jolla.~A.rpm"
   "opt-qt5-qtwebchannel-5.15.10+kde3-1.3.1.jolla.~A.rpm"
   "opt-qt5-qtwebsockets-5.15.10+kde2-1.3.1.jolla.~A.rpm"
   "opt-qt5-qtwebview-5.15.10-1.3.2.jolla.~A.rpm"
   "opt-qt5-rpm-macros-5.15.10-1.4.1.jolla.noarch.rpm"
   "opt-qt5-sfos-maliit-platforminputcontext-1.0.1-1.1.4.jolla.~A.rpm" ; armv7hl
   "opt-qt5-sfos-maliit-platforminputcontext-1.0.1-1.1.3.jolla.~A.rpm" ; aarch64
   "qqc2-breeze-style-5.27.4+git1-1.2.8.jolla.~A.rpm" ; armv7hl
   "qqc2-breeze-style-5.27.4+git1-1.2.7.jolla.~A.rpm" ; aarch64
   "qt-runner-0.4.0-1.6.1.jolla.~A.rpm"))

(defun cc (&rest args)
  (apply 'concatenate 'string args))
 
(defun run ()
  (let ((to-dir (format nil "rpms/~A/" *arch*))
        (to-dir-devel (format nil "rpms/~A-devel/" *arch*)))
    (ensure-directories-exist to-dir)
    (dolist (file *files*)
      (let* ((noarch (search "noarch" file))
             (devel (search "devel" file))
             (url (format nil *url* *arch* (if noarch "noarch" *arch*)))
             (rpm (if noarch
                      file
                      (format nil file *arch*)))
             (to-dir* (if devel to-dir-devel to-dir)))
        (unless (probe-file (cc to-dir* rpm))
          (let ((s (ext:run-program "wget" (list (cc url rpm) "-P" to-dir*))))
            (loop :for line = (read-line s nil nil)
                  :while line :do (princ line) (terpri)
                  :finally (close s))))))))

(run)

