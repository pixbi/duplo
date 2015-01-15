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
  $MOCHA_BROWSER $DUPLO_TARGET/index.html -R html-cov > $DUPLO_TARGET/coverage.html
  echo "Succeeded. 'public/coverage.html' generated"
}

show_coverage_summary () {
  $DUPLO_UTIL/cov-summary.js $DUPLO_TARGET/coverage.html
}

# TODO: refactor and move this to `Scripts.hs` when proven to work
echo "(function () {" >> $DUPLO_CWD"/public/index.js"
cat $DUPLO_PATH"/etc/test/vendor/chai.js" >> $DUPLO_CWD"/public/index.js"
echo ";" >> $DUPLO_CWD"/public/index.js"
cat $DUPLO_PATH"/etc/test/vendor/sinon.js" >> $DUPLO_CWD"/public/index.js"
echo ";" >> $DUPLO_CWD"/public/index.js"
cat $DUPLO_PATH"/etc/test/vendor/mocha.js" >> $DUPLO_CWD"/public/index.js"
echo ";" >> $DUPLO_CWD"/public/index.js"
cat $DUPLO_PATH"/etc/test/vendor/runner.js" >> $DUPLO_CWD"/public/index.js"
echo ";" >> $DUPLO_CWD"/public/index.js"
find /Users/kenhkan/Desktop/Codebase/pixbi/embed/public/tests/ -name "*.js" | xargs cat >> /Users/kenhkan/Desktop/Codebase/pixbi/embed/public/index.js
echo "}).call(window);" >> $DUPLO_CWD"/public/index.js"

# Main processes

# TODO: generate coverage when proven to work
#gen_coverage
run_headless && run_cross_browsers
#show_coverage_summary
