#!/usr/bin/env node

var amdclean = require('amdclean');
var readline = require('readline');
var code = "";

var rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
});

rl.on('line', function(line) {
  code += line + "\n";
});

rl.on('close', function() {
  console.log(amdclean.clean(code));
  code = "";
});
