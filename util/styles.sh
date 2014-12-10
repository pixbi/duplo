#!/usr/bin/env bash

cat | $DUPLO_NODEJS"stylus" | $DUPLO_NODEJS"autoprefixer" | $DUPLO_NODEJS"cleancss" | cat
