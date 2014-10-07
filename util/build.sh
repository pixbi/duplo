#!/bin/sh
mkdir -p .shake
ghc --make app/src/Main.hs -rtsopts -with-rtsopts=-I0 -outputdir=.shake -o .shake/build && .shake/build "$@"
