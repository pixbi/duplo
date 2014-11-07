#!/usr/bin/env bash

src=$1
dest=$2

cp -anv $1 $2

# Need this for Shake. `cp -n` returns status code `1` on any file not
# overwritten.
exit 0
