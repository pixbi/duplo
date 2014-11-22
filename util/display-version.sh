#!/usr/bin/env bash

root=$1

# Gotta love Cabal's manifest format!
cat $root"/duplo.cabal" |
  # Get the version line
  grep "^version:" |
  # Extract the version
  awk -F":" '{print $2}' |
  # Remove spaces
  tr -d ' ' |
  # Proper display
  awk '{ print "duplo v" $0; }'
