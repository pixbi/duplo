# duplo

Intuitive, simple building blocks for building composable, completely
self-managed web applications


## Installation

    $ npm install -g duplo


## Usage

* `duplo new <name> <git-repo-url>` scaffolds a new duplo repo
* `duplo dev` starts a local server and re-compiles on file change
* `duplo build` runs a build. This could be used for checking the code against
  Closure Compiler.
* `duplo patch` builds the project and bump the patch version
* `duplo minor` builds the project and bump the minor version
* `duplo major` builds the project and bump the major version


## Principles

1. Stay as close to raw JavaScript as possible, because JavaScript lacks the
   mechanism to build strong abstraction.
2. Closures are discouraged. Optimally, there should be no function, anonymous
   or otherwise, nested within another function. A callback should be named
   explicitly to another static function.
3. `this` is evil. Developers should not need to context-switch between
   functions.
4. One file contains one module that does one thing.


## Technologies

* [Jade](http://jade-lang.com/) over HTML
* [Stylus](http://learnboost.github.io/stylus/) over CSS
* JavaScript


## File Structure

    app/            --> Application code
    app/index.jade  --> Entry point for templates. Only this file is compiled.
                        Use Jade's include system to pull in other templates.
    app/index.json  --> Optional parameter object made available as `APP`
    app/assets/     --> Asset files are copied as-is to build's top-level
                        directory
    app/styl/       --> Any application style (see below for details)
    app/modules/    --> Module within the application that are included AFTER
                        code in the top-level `app/` directory when building
    components/     --> Other repos imported via Component.IO
    component.json  --> The Component.IO manifest
    dev/            --> Any code necessary to run the application in dev mode
    public/         --> Built files when developing. Not committed to source
    test/           --> Test files go here


## Development

During development, everything in the `dev/` directory is copied over as-is *at
the end* of the build process. This means that files in the directory would
replace whatever is built at their respective locations. The `index.html` in
`dev/` would need to reference the script and the tag manually, e.g.

    <html>
      <head>
        <link rel="stylesheet" href="style.css"/>
      </head>
      <body>
        <script src="script.js"></script>
      </body>
    </html>


## Mode

The `MODE` free variable is exposed in `app/index.js`. When in development,
`MODE === 'dev'` should be `true`.


## Dependency Management

One declares a dependency by calling `require(2)`. It is not unlike CommonJS,
but the similarity ends there. There is neither `exports` nor `module`.
Instead, all variable and functions global to that module are always exported.

```js
// a.js
var count = 0;

function addOne (x) {
  count++;
  return x + 1;
}

function addNone (x) {
  count++;
  return x;
}

// b.js
var a = require('a');

a.addOne(10); // === 11
a.addNone(10); // === 10
a.count // === 2
```

### Module Path

There is actually something incomplete in the above example: the first
parameter to `require(2)` must be a "module path". The path consists of the
repo in which the module lives, as referenced by its Component.IO name,
followed by its path in its location relative to its module's `app/modules/`
directory.

For example, if the repo running duplo is `pixbi/war`, as per Component.IO
convention:

```js
// components/pixbi-nuclear-missile/app/modules/dod/pentagon.js
function launch () {
  // World annihilation
}

function countdown (count) {
  setTimeout(launch, count);
}

// app/modules/white-house/president.js
var pentagon = require('pixbi.nuclearMissile.dod.pentagon');

function destroy () {
  pentagon.countdown(10000);
}

// app/index.js
var president = require('pixbi.war.whiteHouse.president');

function main () {
  president.destroy();
}
```

Yes, it is relatively verbose to call another module. The point is to be
explicit as possible and to encourage shorter module names when naming the
module files.

### Factory Pattern

The factory pattern is implemented at the module level to avoid having to
implement it at the application level, as it is a common pattern.

A module is instantiated by specifying a name to `require(2)` as the second
parameter. Take the following example:

```js
// a.js
var x = 0;

function incrementBy (y) {
  x += y;

  return x;
}

// b.js
var a = require('user.repo.a', 'someName');

a.incrementBy(1); // -> 1

// c.js
var a = require('user.repo.a', 'someOtherName');

a.incrementBy(2); // -> 2

// d.js
var a = require('user.repo.a', 'someName');

a.incrementBy(3); // -> 4

// e.js
var a = require('user.repo.a');

a.incrementBy(4); // -> 4
```

Note that when the second parameter is absent, it "names" the instance as an
empty string. In effect, all modules are singletons by default and optionally
instantiable by name.

#### The Messy Factory

Question: how do you create dynamic instances?

Short answer: You don't. The reason behind this strictness on instance creation
is because of the complexity that the factory pattern introduces. Take the
following example, per the conventional JavaScript prototypal model:

Say a `new Car()` is instantiated; an `new Engine()` is then instantiated as a
result. The conventional approach is that the new engine can only be referenced
from within the car instance. This provides run-time isolation.

The flip-side of this is that potentally a complicated tree of dynamic objects
are interconnected in a way that is difficult to hold in a person's working
memory. The result is typically an unmanageable codebase.

An alternative approach is to make everything static. Instead of dynamically
instantiating objects (in this case, modules), you would name your instance. In
duplo, you write `var a = require('a.b.c', 'some-name');`.

The primary issue with this approach is that any two modules could be referring
to the same instance of another module without knowing that someone else is
doing the same. This is a typical source of bugs as this introduces global
state. However, this is also the only way to provide shared state without
passing around instances via a global mediator. Global state exists either way,
but the latter has a complexity cost due to its dynamic nature.

#### The Factory Solution

So back to the original question: how do you create dynamic instances, for
cases where you need a new engine in a new car?

The answer is still: you don't. You embrace the functional programming
philosophy, that modules, given that they are simply functions, are units of
pure computation. Think of the module as a "code template". It dictates how
things are computed given certain inputs. An instance, on the other hand, is
*NOT* a concrete form of this template, as you would expect in object-oriented
programming, but a specific scenario where this template applies.

Example: an engine could be a module. An engine for a car and an engine for a
private jet would then be instances of the module. The two instances are
behaviorally identical but serve slightly different purposes.

The reason against passing any data from the caller to the module instance is
to avoid needless dynamicity. A module should perform one thing and only one
thing. The purpose of instantiation is only to allow state to be separated
statically.

### The Reactor Pattern

Another common pattern used in frontend web development is the reactor pattern,
known to web developers as `addEventListner(2)` and friends. This pattern is
also implemented at the module level.

Every module object, when required, contains an `addEventListener(2)`, a
`removeEventListener(2)`, and a `dispatchEvent(1)`. All these functions are
available as free variables within the module. Note that they are different
from the conventional DOM Event API. These are the custom event API used in
duplo:

```js
// a.js

function remove (listener) {
  removeEventListener('launch', listener);
}

function launch () {
  dispatchEvent({
    arg: 'must be an object as there is only one argument allowed'
  });
}

// b.js
var a = require('a');

function listener (arg) {
  arg; // == 'must be an object...'
}

a.addEventListener('launch', listener);

a.launch();

a.removeEventListener('launch', listener);
// is the same as
a.remove(listener);

```


## Entry Point

Every application has a main entry point. In a duplo application, it is
`app/index.js`. The content of this file is executed upon the
`DOMContentLoaded` event is fired.

Each repo may contain its own `app/index.js` but only the repo in which duplo
is run is its `app/index.js` executed. `app/index.js` files of other repos that
are pulled into the top-level repo via Component.IO are ignored.

## Application Parameters

You may specify an optional `assets/app.json` (or `dev/app.json` in
development, the content of which would be available to `app/index.js` as a
free variable named `APP`. For instance, with a `params.json` of:

```json
{
  "config": {
    "kickass": true
  }
}
```

You may do this in `app/index.js`:

```js
APP.config.kickass === true;
```

The benefit of this is that we could place a `app.json` in `dev/` for dev mode
and one in `assets/` for production and have a complete isolation between code
And configuration.


## CSS/Stylus Order

Where you place your CSS files within `app/` is significant. Stylus files will
be concatenated in this order:

    app/styl/variables.styl   --> An optional variable file that gets injected
                                  into every Stylus file
    app/styl/keyframes.styl   --> Keyframes
    app/styl/fonts.styl       --> Font declarations
    app/styl/reset.styl       --> Resetting existing CSS in the target
                                  environment
    app/styl/main.styl        --> Application CSS that goes before any module
                                  CSS
    app/modules/**/index.styl --> CSS relevant to specific modules


## Selective Exclusion

Some cases require the repo to be polymorphic in the sense that we could
generate different forms of the same codebase. For example, you may need to
build the repo in an embeddable form which would exclude certain dependencies
that are required in its standalone form. In this case you would include an
`exclude` attribute in the `component.json` manifest file. The `forms` object
then contains the `embeddable` and the `standalone` attributes, each of which
then contains an array of dependencies as specified in the `dependencies`
attribute to *exclude*.

Running `duplo build embeddable` would build without the specified dependencies
under `embeddable` while running `duplo build standalone` would do the same
with those specified under the `standalone` attribute. `duplo build` would
build with all dependencies.

Note that selective exclusion applies at the dependency level but not files in
the component.

An example of a `component.json`:

```json
{
  "dependencies": {
    "pixbi/sdk": "1.1.1",
    "pixbi/embeddable": "2.2.2",
    "pixbi/standalone": "3.3.3"
  },
  "exclude": {
    "embeddable": [
      "pixbi/standalone"
    ],
    "standalone": [
      "pixbi/embeddable"
    ]
  }
}
```


## In-Depth Explanation

The following sections are explanations for how duplo works. Feel free to skip
if knowing the implementation details do not bother you, although understanding
how duplo works under the hood helps with debugging by offering a mental model
of where everything goes.

### Compiling

Compiling the project performs these steps:

1. Copy files in `app/assets/` to `public/`
2. Copy files in `dev/` to `public/`
3. `public/index.html` is created if it doesn't already exist
4. Compile all Stylus files (order specified in the [order
   section](#cssstylus-order)) under `app/` and concatenate into one CSS file
   as `public/index.css`
5. Concatenate all JavaScript under `app/` into one JS file as
   `public/index.js`
6. Compile all Jade files under `app/` into one HTML file and inject into the
   end of `body` in `public/index.html`
7. Write into `public/index.html` tags to include `style.css` and `script.js`

### Building

Building the project performs these steps:

1.  Stash changes to git to avoid data loss (you should of course make sure
    there is no uncommitted code as well)
2.  Checkout the `develop` branch
3.  Compile the project
4.  Apply Closure Compiler with advanced optimizations on the built JavaScript.
    Any error would stop the process here and fixes should be applied before
    retrying.
5.  Apply any transformation for further uglification
6.  Scan through the `app/` directory and write all relevant file references to
    `components.json`
7.  Bump the respective version
8.  Commit to git
9.  Checkout the `master` branch and merge `develop` into `master`
10. Apply the new version as a new git tag
11. Checkout the `develop` branch again


## Copyright and License

Code and documentation copyright 2014 Pixbi. Code released under the MIT
license. Docs released under Creative Commons.
