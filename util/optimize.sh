#!/usr/bin/env bash

echo "$(cat)" | $DUPLO_UTIL"amd.js" | $DUPLO_NODEJS"uglifyjs" -m -c | cat
