#!/bin/sh

if [ -e manage.py -a -x manage.py ]; then
    command=$(pwd)/manage.py
elif [ -n "$VIRTUAL_ENV" ]; then
    command=$(find $VIRTUAL_ENV -type f -name manage.py -print | grep -v 'lib/python' | head -n 1)
else
    command=""
fi

if [ -z "$command" ]; then
    echo "N"
    echo "Could not find manage.py command."
    exit
fi

newpath=$(echo 'import sys; print >>sys.stderr, ":".join(sys.path)' | $command shell 2>&1 | grep ':/')

if [ -z "$newpath" ]; then
    echo "N"
    echo "Problem parsing output of command."
    exit
else
    echo "Y"
    echo $newpath
fi
