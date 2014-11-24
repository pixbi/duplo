#!/usr/bin/env bash

src=$1
dest=$2

# Copy boilerplate
cp -anv "$src"* "$dest"
# Remove all hidden files
find . -name ".*" -type f | xargs rm
# Copy gitignore file
cp -n "$src"/.gitignore "$dest"

# Need this for Shake. `cp -n` returns status code `1` on any file not
# overwritten.
exit 0
