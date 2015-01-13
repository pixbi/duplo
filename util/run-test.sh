#!/usr/bin/env bash

MOCHA_PHANTOMJS=$DUPLO_PATH/node_modules/.bin/mocha-phantomjs
BROWSERSTACK=$DUPLO_PATH/node_modules/.bin/browserstack-runner

JSCOVERAGE=$DUPLO_PATH/node_modules/.bin/jscoverage
MOCHA_BROWSER=$DUPLO_PATH/node_modules/.bin/mocha-browser

RUNNER_PATH=public/index.html


run_headless () {
  $MOCHA_PHANTOMJS $RUNNER_PATH
}

run_cross_browsers () {
  echo "Beginning cross-browser testing..."
  if [ -a ./browserstack.json ]; then
    cp -r ./browserstack.json public/browserstack.json && cd public && $BROWSERSTACK --verbose
  else
    echo "Skipping. Could not get browserstack.json from your project."
  fi
}

gen_coverage () {
  echo "Generating coverage page..."
  $JSCOVERAGE public/tests public/tests --exclude ".*"
  $MOCHA_BROWSER public/index.html -R html-cov > public/coverage.html
  echo "Succeeded. 'public/coverage.html' generated"
}

show_coverage_summary () {
  $DUPLO_UTIL/cov-summary.js public/coverage.html
}


# Main processes
gen_coverage
run_headless && run_cross_browsers
show_coverage_summary
