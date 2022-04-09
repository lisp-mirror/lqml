#!/usr/bin/env python3

import os

secs = 0

for root, dirs, files in os.walk('qml'):
    for name in files:
        if name.endswith(".qml"):
            secs += int(os.path.getctime(os.path.abspath(os.path.join(root, name))))

print("Content-type:text/plain\r\n\r\n" + str(secs))
