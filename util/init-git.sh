#!/usr/bin/env bash

name=$1

# Of course we need a commit first
git init
git add . --all
git commit -m 'Initial'
# Make sure we have a primary remote repository
git remote add origin "git@github.com:"$name".git"
# Create branch for production
git checkout -b live
