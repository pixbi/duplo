#!/usr/bin/env bash

# Jade takes a FILE within the directory that would be the path. I know, weird.
cat | $DUPLO_NODEJS"jade" --pretty --path $DUPLO_CWD"/index.jade" | cat
