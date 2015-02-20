#!/usr/bin/env bash

MOCHA_PHANTOMJS=$DUPLO_NODEJS/mocha-phantomjs
BROWSERSTACK=$DUPLO_NODEJS/browserstack-runner

JSCOVERAGE=$DUPLO_NODEJS/jscoverage
MOCHA_BROWSER=$DUPLO_NODEJS/mocha-browser

RUNNER_PATH=$DUPLO_TARGET/index.html


run_headless () {
  $MOCHA_PHANTOMJS $RUNNER_PATH
}

run_cross_browsers () {
  echo "Beginning cross-browser testing..."
  if [ -a $DUPLO_CWD/browserstack.json ]; then
    cp -r $DUPLO_CWD/browserstack.json $DUPLO_TARGET/browserstack.json && cd $DUPLO_TARGET && $BROWSERSTACK --verbose
  else
    echo "Skipping. Could not get browserstack.json from your project."
  fi
}

gen_coverage () {
  echo "Generating coverage page..."
  $JSCOVERAGE $DUPLO_TARGET/tests $DUPLO_TARGET/tests --exclude ".*"
  echo "jscoverage done"
  $MOCHA_BROWSER $DUPLO_TARGET/index.html -R html-cov > $DUPLO_TARGET/coverage.html
  echo "Succeeded. 'public/coverage.html' generated"
}

show_coverage_summary () {
  $DUPLO_UTIL/cov-summary.js $DUPLO_TARGET/coverage.html
}

# TODO: refactor and move this to `Scripts.hs` when proven to work
TAR_PATH=$DUPLO_CWD/public/index.js

build_deps () {
  local dep=$DUPLO_CWD/public/index.dep.js
  local tar=$DUPLO_CWD/public/index.js
  local tmp=$DUPLO_CWD/public/index.tmp

  echo "(function () {" >> $dep
  echo "var __module__ = module, module = undefined;" >> $dep
  cat $DUPLO_PATH"/etc/test/vendor/sinon.js" >> $dep
  echo ";" >> $DUPLO_CWD"/public/index.js"
  echo "module = __module__;" >> $dep
  cat $DUPLO_PATH"/etc/test/vendor/chai.js" >> $dep
  echo ";" >> $DUPLO_CWD"/public/index.js"
  cat $DUPLO_PATH"/etc/test/vendor/mocha.js" >> $dep
  echo ";" >> $DUPLO_CWD"/public/index.js"
  cat $DUPLO_PATH"/etc/test/head.js" >> $dep
  echo ";" >> $DUPLO_CWD"/public/index.js"
  echo "}).call(window);" >> $dep

  cat $dep > $tmp
  cat $TAR_PATH >> $tmp
  mv $tmp $TAR_PATH
  rm -f $dep
}

build_tests () {
  echo "(function () {" >> $TAR_PATH
  for i in $(find $DUPLO_CWD"/public/tests/" -name "*.js"); do
    echo ";" >> $TAR_PATH
    cat $i >> $TAR_PATH
  done
  echo "}).call(window);" >> $TAR_PATH
}

# Main processes
### build
build_deps
build_tests

### run
for i in "$@"; do
  case $i in
    --no-runner) exit ;;
    *) exit ;;
  esac
done

gen_coverage
run_headless && run_cross_browsers
show_coverage_summary
