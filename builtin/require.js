
var module = {};

function defineModule (name, ns, initFn) {
  if (!module[name]) {
    module[name] = {};
  }
  module[name][ns] = {
    'inited': false,
    'fn': initFn,
    'exports': {}
  };
}

function try_require (name, ns) {
  var m = module[name][ns];
  if (!m) {
    throw new Error('could not load module: ' + name + ns);
  }
  if (!m.inited) {
    m.inited = true;
    m.fn(m, m.exports);
  }
  return m.exports;
}

function require (_path) {
  
  // load local module
  if (!_path) {
    _path = 'index';
  }
  if (module[app.name][_path]) {
    return try_require(app.name, _path);
  }
  if (module[app.name][_path + '.index']) {
    return try_require(app.name, _path + '.index');
  }

  // load dependency
  var pathObj = _path.split('.');
  var name = pathObj[1];
  var ns = pathObj.slice(2).join('.');
  var notfoundErr = new Error('could not load module: ' + _path);
  
  if (!ns) {
    ns = 'index';
  }
  if (!module[name]) {
    throw notfoundErr;
  }
  if (module[name][ns]) {
    return try_require(name, ns);
  }
  if (module[name][ns + '.index']) {
    return try_require(name, ns + '.index');
  }
  throw notfoundErr;
}

window.require = require;
