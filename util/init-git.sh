#!/usr/bin/env bash

name=$1

git init
git add . --all
git commit -m 'Initial'
git remote add origin "git@github.com:"$name".git"
