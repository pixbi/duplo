#!/usr/bin/env bash

path=$1
pattern=$2

if [ -d "$path" ]; then
  find "$path" -follow -not -name ".*" -name "$pattern" -type f | cat
fi
