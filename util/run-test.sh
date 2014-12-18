#!/usr/bin/env sh

MOCHA_PHANTOMJS=$1/node_modules/.bin/mocha-phantomjs
RUNNER_PATH=public/index.html

run_headless () {
  $MOCHA_PHANTOMJS $RUNNER_PATH
}

run_cross_browsers () {
  echo 'TODO(Yorkie): to be implemented'
}

# main process
run_headless
run_cross_browsers
