#!/bin/sh
mkdir -p _shake
ghc --make app/src/Main.hs -rtsopts -with-rtsopts=-I0 -outputdir=_shake -o _shake/build && _shake/build "$@"
