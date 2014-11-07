#!/usr/bin/env bash

src=$1
dest=$2

# Copy template
cp -anv $src/* $dest
# Copy gitignore file
cp -n $src/.gitignore $dest

# Need this for Shake. `cp -n` returns status code `1` on any file not
# overwritten.
exit 0
