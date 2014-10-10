#!/usr/bin/env bash

# Paths
dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
root="$( cd "$dir""/../" && pwd )"

# Default values
appMode="dev"
appParams="{}"


display_help() {
  cat <<-EOF

  Usage: duplo [commands] [args]

  Commands:

    duplo new <name> <repo-url>   scaffolds a new duplo repo
    duplo dev                     starts a local server and re-compiles on file
                                  change
    duplo build                   runs a build. This could be used for checking
                                  the code against Closure Compiler.
    duplo patch                   builds the project and bump the patch version
    duplo minor                   builds the project and bump the minor version
    duplo major                   builds the project and bump the major version

EOF
  exit 0
}

precommit() {
  git stash
  git checkout develop
}

commit() {
  ver='0.0.0'
  set_version $cwd component ver

  # Commit
  git add component.json
  git commit -m 'Bump version'
  # Merge into master
  git checkout master
  # Always force the new changes
  git merge develop -X theirs
  # Apply tag
  git tag $ver
  ## Sync with Github
  git push origin develop:develop
  git push origin master:master
  git push origin --tags
  # Go back to develop
  git checkout develop
}

display_version() {
  echo 'duplo v'$version
}


case "$1" in
  version|ver|-v|--ver|--version)
    display_version
    ;;

  *)
    echo $root
    echo $appMode
    echo $appParams
    # Pass control to compiler with path to duplo root directory, mode, and
    # application paramters. Forward all arguments
    DUPLO_PATH="$root" APP_MODE="$appMode" APP_PARAMS="$appParams" \
      "$root"/dist/build/duplo/duplo $*
    ;;

esac
