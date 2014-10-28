#!/bin/sh
mkdir -p .shake
ghc --make src/Main.hs -rtsopts -with-rtsopts=-I0 -outputdir=.shake -o .shake/build && .shake/build "$@"
