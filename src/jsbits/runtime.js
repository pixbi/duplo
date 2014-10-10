// The duplo runtime
//
// PRE-CONDITION
// =============
//
// Assume a free variable `APP` with attributes `params`, `mode`, and module
// definitions.
//
//
// POST-CONDITION
// ==============
//
// `APP` would be removed and `require(2)` made available.

var require = (function setup () {
  // Contains all modules
  var modules = {};
  // Contains instances of modules
  var instances = {};
  var _modules, keys, name, constructor, i, l;

  // Save references to app parameters and mode
  var params = APP._params;
  var mode = APP._mode;

  function addEventListener (allListeners, name, listener) {
    var listeners = allListeners[name];

    // Re-assign with init value if not initialized
    allListeners[name] = listeners = listeners || [];

    listeners.push(listener);
  }

  function removeEventListener (allListeners, name, listener) {
    var listeners = allListeners[name];
    var lsnr, i, l;

    // Quit if it's not there to begin with
    if (! listeners) {
      return;
    }

    // Just go through all the listeners one by one
    for (i = 0, l = listeners.length; i < l; i++) {
      lsnr = listeners[i];

      // The listener needs to match or if there's no listener provided
      if (listener && lsnr === listener) {
        // Remove the listener
        listeners.splice(i, 1);
      }
    }
  }

  function dispatchEvent (allListeners, name, evt) {
    var listeners = allListeners[name];
    var lsnr, i, l;

    // Is there even anyone listening?
    if (! listeners) {
      return;
    }

    // Just go through all the listeners one by one
    for (i = 0, l = listeners.length; i < l; i++) {
      listeners[i](evt);
    }
  }

  // The `require(2)` function
  function require (moduleName, instanceName) {
    var construct = modules[moduleName];
    // Concatenate module name and instance name to form a unique name
    name = moduleName + '_' + (instanceName || "");
    var instance = instances[name];

    // Instantiate if it doesn't exist
    if (! instance) {
      instance = instances[name] = construct();
    }

    return instance;
  }

  // Meta-constructor that enhances normal constructors with the reactor
  // pattern
  function god (constructor) {
    var listeners = {};

    // Bind, the fast way
    var ael = function (name, listener) {
      return addEventListener(listeners, name, listener);
    };
    var rel = function (name, listener) {
      return removeEventListener(listeners, name, listener);
    };
    var de = function (name, evt) {
      return dispatchEvent(listeners, name, evt);
    };

    return function construct () {
      // Expose the application mode, parameters, and curried reactor functions
      // to module closure
      return new constructor(mode, params, ael, rel, de);
    };
  }

  // Go through all modules
  _modules = APP.modules;
  keys = Object.keys(_modules);
  for (i = 0, l = modules.length; i < l; i++) {
    name = keys[i];
    constructor = _modules[name];

    // Register the module
    modules[name] = god(constructor);
  }

  // Expose only `require(2)`
  return require;
})();

// Clean up
APP = null;
