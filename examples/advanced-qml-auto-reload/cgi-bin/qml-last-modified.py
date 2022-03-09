#!/usr/bin/env python3

import os

total       = 0
max_secs    = 0
edited_file = ''

for root, dirs, files in os.walk('qml'):
    for name in files:
        if name.endswith('.qml'):
            current_file = os.path.join(root, name)
            secs = int(os.path.getctime(os.path.abspath(current_file)))
            total += secs
            if (secs > max_secs):
                max_secs = secs
                edited_file = current_file

print('Content-type:text/plain\r\n\r\n')
print(str(total) + '\r\n' + edited_file[4:])
