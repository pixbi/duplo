#!/usr/bin/env bash

cat |
  $DUPLO_NODEJS"html-minifier" --minify-css --minify-js --collapse-whitespace |
  cat
