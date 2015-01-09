#!/usr/bin/env node

var fs = require('fs');
var cheerio = require('cheerio');

fs.readFile(process.argv[2], function (err, data) {
  if (err) throw err;
  putSummary(cheerio.load(data + ''));
});

function putHeader (text) {
  console.log('\033[1;30m%s\033[0m\n', text);
}

function putLine (path, val, level) {
  if (level === 'high')
    console.log('\033[0;32m   %s: %s\033[0m', path, val);
  else if (level === 'medium')
    console.log('\033[0;33m   %s: %s\033[0m', path, val);
  else if (level === 'low')
    console.log('\033[0;31m   %s: %s\033[0m', path, val);
  else if (level === 'terrible')
    console.log('\033[1;31m   %s: %s\033[0m', path, val);
}

function putSummary ($) {
  var total = $('#stats>div.percentage').html();
  putHeader('>> Coverage Summary (' + total + ')');

  $('#menu>li').each(function () {
    var span = $(this).children('span');
    if (!span || span.length === 0)
      return;
    var tag = span.attr('class').split(' ')[1];
    var val = span.html() + '%';
    var path = $(this).children('a').text();
    putLine(path, val, tag);
  });
}
