#!/usr/bin/env sh

MOCHA_PHANTOMJS=$1/node_modules/.bin/mocha-phantomjs
BROWSERSTACK=$1/node_modules/.bin/browserstack-runner

JSCOVERAGE=$1/node_modules/.bin/jscoverage
MOCHA_BROWSER=$1/node_modules/.bin/mocha-browser

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

gen_coverage () {
  $JSCOVERAGE public/index.js public/index.js
  $MOCHA_BROWSER index.html -R html-cov > coverage.html
  echo "successed, public/coverage generated"
}

# main process
run_headless && run_cross_browsers && gen_coverage
