#!/usr/bin/env bash

version=$1

# Always update the local branch first; otherwise we may have merging issue
# with production later on.
git pull origin master
# Commit to master.
git add component.json
git commit -m "Bump version to v"$version
# Merge into production branch.
git checkout production
# Update local production branch first.
git pull origin production
# Always force the new changes from master because the master branch should
# always be ahead.
git merge master -X theirs
# Apply tag for release.
git tag $version
# Sync both branches and the new tag with remote.
git push origin production:production
git push origin master:master
git push origin --tags
# Continue working on master.
git checkout master
