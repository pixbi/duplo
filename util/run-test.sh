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
  echo "generating coverage page..."
  $JSCOVERAGE public/app/modules public/app/modules
  $JSCOVERAGE public/test/modules public/test/modules
  $MOCHA_BROWSER public/index.html -R html-cov > public/coverage.html
  echo "successed, public/coverage generated"
}

# main process
gen_coverage
run_headless && run_cross_browsers
