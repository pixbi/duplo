#!/usr/bin/env bash

cabal sandbox init
cabal configure
cabal install
cabal build
