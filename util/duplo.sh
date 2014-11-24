#!/usr/bin/env bash

# NOTE: the heavy-lifting should be handled by Shake. This is simply a tiny
# entry point to handle actions that Shake does not excel in, such as starting
# long-running tasks like a web server. Ultimately we want this to be in
# Haskell as well. This is just easier to get started.

## Arguments
#cmd=$1
#arg1=$2
#arg2=$3
#arg3=$4

# Defaults

#if [ -z "$PORT" ]; then
#  PORT=8888
#fi

#if [ -z "$DUPLO_ENV" ]; then
#  DUPLO_ENV=dev
#fi

## Common paths
#cwd="$( pwd )"

## Get the full path, even if the file is run a symlink and/or with a relative
## path
#path=`perl -e 'use Cwd "abs_path"; print abs_path(shift)' $0`
## Take the directory of that
#dir="$( dirname "$path" )"
## Get the project's root
#root="$( cd "$dir"/../ && pwd )"

## Should we go into infinite loop?
#forever=false

## Do we want extra info?
#if [ "$2" = "--verbose" ]; then
#  verbose="true"
#else
#  verbose="false"
#fi


## Construct the command to invoke duplo
#function make_duplo_cmd() {
#  echo env \
#       #CWD="$cwd" \
#       #DUPLO_PATH="$root" \
#       #DUPLO_ENV="$DUPLO_ENV" \
#       #DUPLO_MODE="$DUPLO_MODE" \
#       #DUPLO_IN="$DUPLO_IN" \
#       #DUPLO_VERBOSE="$verbose" \
#       #DUPLO_BUMP_LEVEL="$DUPLO_BUMP_LEVEL" \
#       #"$root"/dist/build/duplo/duplo "$cmd" "$arg1" "$arg2" "$arg3" --verbose
#}

## Display duplo's version
#function display_version() {
#  echo ""
#  # Love Cabal's manifest format!
#  cat $root"/duplo.cabal" |
#    # Get the version line
#    grep "^version:" |
#    # Extract the version
#    awk -F":" '{print $2}' |
#    # Remove spaces
#    tr -d ' ' |
#    # Proper display
#    awk '{ print "duplo v" $0; }'
#}

## Always display duplo version
#display_version

# Perform any setup and actions that Shake is not good at.
case "$cmd" in

  ## Normalize
  #info|version|ver|-v|--ver|--version)
  #  cmd=version
  #  ;;

  ## Patch by default
  #bump|release)
  #  DUPLO_BUMP_LEVEL=patch
  #  cmd=bump
  #  ;;

  ## Specified bump
  #patch|minor|major)
  #  DUPLO_BUMP_LEVEL=$cmd
  #  cmd=bump
  #  ;;

  # A server is needed
  dev|live)
    ## Only when environment isn't provided.
    #if [ -z "$DUPLO_ENV" ]; then
    #  DUPLO_ENV=$cmd
    #fi

    #cmd=build

    ## The server
    #$root/dist/build/server/server $PORT &

    ## The watchers
    #$root/bin/spy run "$( make_duplo_cmd )" $cwd/app/ -n &
    $root/bin/spy run "$( make_duplo_cmd )" $cwd/dev/ -n &
    $root/bin/spy run "$( make_duplo_cmd )" $cwd/components/ -n &

    ## Kill all child processes
    #function cleanup() {
    #  kill $(jobs -p)
    #  exit $?
    #}

    ## Trap Ctrl-C
    #trap cleanup SIGINT

    ## Infinite loop
    #forever=true
    #;;

  ## Production
  #build)
  #  DUPLO_ENV=build
  #  ;;

  # Testing forces an environment change
  test)
    DUPLO_ENV=test
    cmd=build
    ;;

  ## Create a new project
  #new|init)
  #  cmd=init

  #  if [ -z "$arg1" -o -z "$arg2" ]; then
  #    echo "There must be a GitHub user and a repo name."
  #    echo ""
  #    exit 1
  #  fi
  #  ;;

  ## Other allowed commands are passed through
  #clean)
  #  ;;

  ## Default to help
  #*)
  #  # Then the help text
  #  cat $root"/etc/help.txt"

  #  exit 0
  #  ;;

esac

## Run build system
#eval "$( make_duplo_cmd )"

## Infinite loop
#if $forever; then
#  while true; do read _; done
#fi
