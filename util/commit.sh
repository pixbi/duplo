#!/usr/bin/env bash

version=$1

# Commit
git add component.json
git commit -m "Bump version to v"$version
# Merge into live
git checkout live
# Always force the new changes
git merge master -X theirs
# Apply tag
git tag $version
# Sync with Github
git push origin live:live
git push origin master:master
git push origin --tags
# Go back to master
git checkout master
