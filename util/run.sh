#!/usr/bin/env bash

# NOTE: the heavy-lifting should be handled by Shake. This is simply a tiny
# entry point to handle actions that Shake does not excel in, such as starting
# long-running tasks like a web server.

# Arguments
cmd=$1

# Defaults
if [ -z "$PORT" ]; then
  PORT=8888
fi

if [ -z "$DUPLO_ENV" ]; then
  DUPLO_ENV=dev
fi

# Common paths
cwd="$( pwd )"
dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
root="$( cd "$dir""/../" && pwd )"


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

# Construct the command to invoke duplo
function make_duplo_cmd() {
  echo env \
       CWD="$cwd" \
       DUPLO_PATH="$root" \
       DUPLO_ENV="$DUPLO_ENV" \
       DUPLO_MODE="$DUPLO_MODE" \
       DUPLO_IN="$DUPLO_IN" \
       DUPLO_BUMP_LEVEL="$DUPLO_BUMP_LEVEL" \
       "$root"/dist/build/duplo/duplo "$cmd"
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

  # A server is needed
  dev|live)
    # Only when environment isn't provided.
    if [ -z "$DUPLO_ENV" ]; then
      DUPLO_ENV=$cmd
    fi

    cmd=build

    # The server
    $root/node_modules/.bin/http-server public -c-1 -p $PORT &

    # The watcher
    $root/node_modules/.bin/watch "$( make_duplo_cmd )" app &

    # Kill all child processes
    function cleanup() {
      kill $(jobs -p)
      exit $?
    }

    # Trap Ctrl-C
    trap cleanup SIGINT

    # Infinite loop
    while true; do read _; done
    ;;

  # Testing forces an environment change
  test)
    DUPLO_ENV="test"
    cmd=build
    ;;

  # Other allowed commands are passed through
  new|build|clean)
    ;;

  # Default to help
  *)
    cat $root"/etc/help.txt"
    exit 0
    ;;

esac


# Run build system
eval "$( make_duplo_cmd )"
