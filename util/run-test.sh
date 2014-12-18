#!/usr/bin/env sh

MOCHA_PHANTOMJS=$1/node_modules/.bin/mocha-phantomjs
BROWSERSTACK=$1/node_modules/.bin/browserstack-runner
RUNNER_PATH=public/index.html

run_headless () {
  $MOCHA_PHANTOMJS $RUNNER_PATH
}

run_cross_browsers () {
  cd public && $BROWSERSTACK
}

# main process
run_headless
run_cross_browsers
