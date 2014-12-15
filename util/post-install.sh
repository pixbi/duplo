#!/usr/bin/env sh

src=node_modules
tar=etc/test/vendor/

# moving files
cp $src/mocha/mocha.js $tar
cp $src/mocha/mocha.css $tar
cp $src/should/should.js $tar
cp $src/sinon/pkg/sinon.js $tar
