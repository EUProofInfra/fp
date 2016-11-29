#!/bin/bash

CMD="/usr/bin/env python3 presubmit-res/presubmit.py"

if hostname -f | grep bham.ac.uk > /dev/null ; then
	if hostname -f | grep cca-\[ul\]g04 > /dev/null ; then
		/bin/tcsh -c "module load python/3.5; module load ghc; env -u LC_CTYPE $CMD"
	else
        echo "ERROR: You must execute this on a lab machine."
        exit 1
    fi
else
	echo Warning: you are not executing this on a lab machine.
	echo Warning: you are not executing this on a lab machine.
	echo Warning: you are not executing this on a lab machine.
	echo
	echo I will continue in 3 seconds.
	sleep 3
	$CMD
fi
