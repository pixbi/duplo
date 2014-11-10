#!/usr/bin/env bash

cabal sandbox init
cabal configure
cabal install --only-dependencies -j8 --reinstall --force-reinstall
cabal build
