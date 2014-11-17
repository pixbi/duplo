#!/usr/bin/env bash

echo "\"use strict\";"
echo "$(cat)" | $DUPLO_UTIL"amd.js" | $DUPLO_NODEJS"uglifyjs" -m -c | cat
