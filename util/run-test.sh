#!/usr/bin/env sh

MOCHA_PHANTOMJS=$1/node_modules/.bin/mocha-phantomjs
BROWSERSTACK=$1/node_modules/.bin/browserstack-runner
RUNNER_PATH=public/index.html

run_headless () {
  $MOCHA_PHANTOMJS $RUNNER_PATH
}

run_cross_browsers () {
  if [ -a ./browserstack.json ]; then
    cp -r ./browserstack.json public/browserstack.json && cd public && $BROWSERSTACK --verbose
  else
    echo "skiped, reason: could not get browserstack.json from your project"
  fi
}

# main process
run_headless && run_cross_browsers
