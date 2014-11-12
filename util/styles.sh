#!/usr/bin/env bash

cat | $DUPLO_NODEJS"stylus" | $DUPLO_NODEJS"cleancss" | cat
