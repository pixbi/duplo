#!/usr/bin/env bash

# NOTE: the heavy-lifting should be handled by Shake. This is simply a tiny
# entry point to handle actions that Shake does not excel in, such as starting
# long-running tasks like a web server.

# Arguments
cmd=$1
port=$PORT

# Common paths
cwd="$( pwd )"
dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
root="$( cd "$dir""/../" && pwd )"

# Command to invoke duplo
duplo="env CWD="$cwd" DUPLO_PATH="$root" .cabal-sandbox/bin/duplo"


# TODO: to be refactored into Shake
commit() {
  local level=$1
  local version=$2

  git stash
  git checkout develop

  # TODO: set version to component.json

  # Commit
  git add component.json
  git commit -m "Bump version"
  # Merge into master
  git checkout master
  # Always force the new changes
  git merge develop -X theirs
  # Apply tag
  git tag $version
  ## Sync with Github
  git push origin develop:develop
  git push origin master:master
  git push origin --tags
  # Go back to develop
  git checkout develop
}

# TODO: to be refactored into Shake
display_version() {
  # TODO: get version from component.json
  local version=$1

  echo "duplo v"$version
}

# TODO: to be refactored into Shake, or to be removed?
run_duplo() {
  mode=$1

  # Pass control to compiler with path to duplo root directory, mode, and
  # application paramters. Forward all arguments
  DUPLO_PATH="$root" PROJ_PATH="$cwd" DUPLO_MODE="$mode" APP_PARAMS="$appParams" \
    "$root"/dist/build/duplo/duplo $builderParams
}


# Perform any setup and actions that Shake is not good at.
case "$cmd" in

  # Normalize
  version|ver|-v|--ver|--version)
    cmd=version
    ;;

  # Patch by default
  bump|release)
    DUPLO_BUMP_LEVEL=patch
    cmd=bump
    ;;

  # Specified bump
  patch|minor|major)
    DUPLO_BUMP_LEVEL=$cmd
    cmd=bump
    ;;

  # Require a web server
  serve|dev|staging|live)
    DUPLO_ENV=$cmd
    cmd=build

    # The server
    node_modules/.bin/http-server public -c-1 -p $port

    # The watcher
    node_modules/.bin/watch "\""$duplo" build\"" app
    ;;

  # Other allowed commands are passed through
  new|help|build|clean)
    ;;

  # Default to help
  *)
    cmd="help"
    ;;

esac


# Run build system
$( $duplo $cmd )
