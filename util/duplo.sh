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

# Get the full path, even if the file is run a symlink and/or with a relative
# path
path=`perl -e 'use Cwd "abs_path"; print abs_path(shift)' $0`
# Take the directory of that
dir="$( dirname "$path" )"
# Get the project's root
root="$( cd "$dir"/../ && pwd )"

# Should we go into infinite loop?
forever=false


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
    $root/dist/build/server/server $PORT &

    # The watchers
    $root/bin/spy run "$( make_duplo_cmd )" $cwd/app/ -n &
    $root/bin/spy run "$( make_duplo_cmd )" $cwd/dev/ -n &
    $root/bin/spy run "$( make_duplo_cmd )" $cwd/components/ -n &

    # Kill all child processes
    function cleanup() {
      kill $(jobs -p)
      exit $?
    }

    # Trap Ctrl-C
    trap cleanup SIGINT

    # Infinite loop
    forever=true
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

# Infinite loop
if $forever; then
  while true; do read _; done
fi
