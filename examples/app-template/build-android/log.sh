# filter for logcat to show only messages from:
# * (qlog ...)       in Lisp
# * console.log(...) in QML

adb logcat -c
adb logcat -s "[LQML]"
