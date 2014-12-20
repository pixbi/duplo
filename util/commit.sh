#!/usr/bin/env bash

version=$1

# Always update the local branch first.
git pull origin master
# Commit to master.
git add component.json
git commit -m "Bump version to v"$version
# Apply tag for release.
git tag $version
# Sync both branches and the new tag with remote.
git push origin master:master
git push origin --tags
# Continue working on master.
git checkout master
