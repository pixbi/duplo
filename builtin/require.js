
var packages = {};

function definePackage (name, ns, initFn) {
  if (!packages[name]) {
    packages[name] = {};
  }
  packages[name][ns] = {
    'inited': false,
    'fn': initFn,
    'exports': {}
  };
}

function try_require (name, ns) {
  var m = packages[name][ns];
  if (!m) {
    throw new Error('could not load package: ' + name + ns);
  }
  if (!m.inited) {
    m.inited = true;
    m.fn(m, m.exports);
  }
  return m.exports;
}

function require (_path) {
  
  // load local package
  if (!_path) {
    _path = 'index';
  }
  if (packages[app.name][_path]) {
    return try_require(app.name, _path);
  }
  if (packages[app.name][_path + '.index']) {
    return try_require(app.name, _path + '.index');
  }

  // load dependency
  var pathObj = _path.split('.');
  var name = pathObj[1];
  var ns = pathObj.slice(2).join('.');
  var notfoundErr = new Error('could not load package: ' + _path);
  
  if (!ns) {
    ns = 'index';
  }
  if (!packages[name]) {
    throw notfoundErr;
  }
  if (packages[name][ns]) {
    return try_require(name, ns);
  }
  if (packages[name][ns + '.index']) {
    return try_require(name, ns + '.index');
  }
  throw notfoundErr;
}

window.require = require;
