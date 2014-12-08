#!/usr/bin/env bash

version=$1

# Commit
git add component.json
git commit -m "Bump version to v"$version
# Merge into production
git checkout production
# Always force the new changes
git merge master -X theirs
# Apply tag
git tag $version
# Sync with Github
git push origin production:production
git push origin master:master
git push origin --tags
# Go back to master
git checkout master
