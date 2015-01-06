#!/usr/bin/env bash

# Copy files
cp -R $DUPLO_CWD/test/ $DUPLO_TARGET/test/
cp -R $DUPLO_CWD/app/  $DUPLO_TARGET/app/

# run dev
sh $DUPLO_UTIL/scripts-dev.sh