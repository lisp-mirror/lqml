#!/bin/sh

for rpm in *.rpm; do
    rpm2cpio $rpm | cpio -idmv
done

devel-su cp -r opt/* /opt/
