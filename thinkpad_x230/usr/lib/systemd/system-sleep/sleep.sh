#!/bin/bash
if [ "$1" = "pre" ]; then
	killall -9 wpa_supplicant #nm-applet bug's workaround
fi

if [ "$1" = "post" ]; then
        /sbin/modprobe -rvf iwldvm
        /sbin/modprobe -rvf iwlwifi
	/sbin/modprobe -v iwldvm
	/sbin/modprobe -v iwlwifi

fi
