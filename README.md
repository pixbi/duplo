# duplo

A opinionated, framework-less build tool for web applications


## Installation

1. Get a [Mac](http://www.apple.com/mac/)
2. Install [Homebrew](http://brew.sh/)
3. Get [npm](http://npmjs.org/): `brew install npm`
4. Get [GMP](https://gmplib.org/): `brew install gmp`
5. Get pre-1.x.x [Component](https://github.com/componentjs): `npm install -g
   component@0.19.9`
6. Get [duplo](https://github.com/pixbi/duplo): `npm install -g duplo`


## Usage

* `duplo help` displays all commands.
* `duplo info` displays the version for this duplo installation.
* `duplo init <user> <repo>` scaffolds a new duplo repo in the current
  directory.
* `duplo build` builds the project. `DUPLO_ENV` defaults to `dev`.
* `duplo dev`: starts a webserver, watches for file changes, and builds in
  development environment.
* `duplo live`: like `duplo dev` but builds in production environment
* `duplo patch` bumps the patch version.
* `duplo minor` bumps the minor version.
* `duplo major` bumps the major version.


## Guiding Principle

This is a build tool, not an application framework. It simply compiles and
builds your codebase for you and does not inject a runtime into or impose a
structure on your application.

However, it does have opinions, specifically:

* [Jade](http://jade-lang.com/) over HTML
* [Stylus](http://learnboost.github.io/stylus/) over CSS
* [GitHub](http://github.com/) and by extension [git](http://git-scm.com/) for
  source code management
* [Heroku](https://www.heroku.com/) for application deployment
* [Selenium](http://docs.seleniumhq.org/) for automated browser testing
* [CircleCI](https://circleci.com/) for continuous integration

The idea is to manage and deploy your code exclusively with git and have
CircleCI deals with deployment for you. However, duplo is a build tool; it
doesn't care about the exact structure of your application. This means that all
scripts are dumped into one single file, and so are the stylesheets and the
markup.


## File Structure

    app/            --> Application code
    app/index.jade  --> Entry point for markups. Only this file is compiled.
                        Use Jade's include system to pull in other markups.
    app/index.js    --> Application entry point. Only the top-level
                        application's `index.js` is included and run. Its
                        dependencies' are ignored.
    app/assets/     --> Asset files are copied as-is to build's top-level
                        directory
    app/styl/       --> It contains "special" stylesheets that get loaded
                        before any other stylesheets.
    app/modules/    --> All other application code not listed above must be
                        placed here. All files at the `app/` level are not
                        included by default.
    components/     --> Other repos imported via Component.IO
    component.json  --> The Component.IO manifest
    dev/            --> Files here are included only when building in
                        development mode.
    dev/assets/     --> Copied as-is just like `app/assets/`. Files here would
                        replace those with the same name under `app/assets/`.
    dev/modules/    --> Works just like `app/modules/`.
    public/         --> Built files when developing. Not committed to source
    test/           --> Test files go here


## Development

During development, everything in the `dev/assets/` directory is copied over
as-is *at the end* of the build process. This means that files in the directory
would replace whatever that has been built (or copied over from `app/assets/`)
at their respective locations.

Anything under `dev/modules/` would be treated just like those under
`app/modules/`, that they would be concatenated/compiled into the respective
output files (i.e. `index.html`, `index.css`, or `index.js`).


## Environment

duplo injects the `DUPLO_ENV` global variable with the value from the
environment variable of the same name when building. There is no default value.


## Entry Point

Every application has a main entry point. In a duplo application, it is
`app/index.js`. Each repo may contain its own `app/index.js` but only the repo
on which duplo is run does duplo execute `app/index.js`. Note that
`app/index.js` is excluded when duplo commits via Component.IO so that the
consuming application does not see library index files when building the
project.

Note that duplo only inspects the *top-level* `define()`. If you use
`require()`, your program may not execute as duplo is not aware of anything
other `define()` declarations. The proper way to declare an entry point in
`app/index.js` is:

```js
define('main', [/* ... dependencies ... */], function (/* ... dependencies ... */) {
  // Code here ...
});
```


## Application Parameterization

If you need some build-time customization of the app, such as customizing each
build with a JSON object of unique IDs and metadata, you can pass any string
as the environment variable `DUPLO_IN`. The string is then turned into a
JavaScript string and stored into a global variable.

To avoid special characters, `DUPLO_IN` must be base-64 encoded.

For example, say you need to pass in a random ID for each build, you would
invoke:

```sh
// Content decoded as: `{"id":"someId"}`
$ env DUPLO_IN="eyJpZCI6InNvbWVJZCJ9" duplo
```

Then in `app/index.js`:

```js
var someId = DUPLO_IN.id;
```

Note that all newline characters are removed before the string is wrapped into
a JavaScript string.


## JavaScript Concatenation Order

JavaScript files are not concatenated in any particular order. You must wrap
code inside an AMD module and declaring its dependencies. For code that needs
to be executed at initialization, utilize the environment's initialization
event such as `document.addEventListener("DOMContentLoaded")` to bootstrap the
rest of the script.


## CSS/Stylus Concatenation Order

Unlike script files, where you place your CSS files within `app/` is
significant. Stylus files will be concatenated in this order:

    app/styl/variables.styl --> An optional variable file that gets injected
                                into every Stylus file
    app/styl/keyframes.styl --> Keyframes
    app/styl/fonts.styl     --> Font declarations
    app/styl/reset.styl     --> Resetting existing CSS in the target
                                environment
    app/styl/main.styl      --> Application CSS that goes before any module
                                CSS
    app/**/*.styl           --> All other CSS files

There is no particular concatenation order between different dependencies.


## HTML/Jade Concatenation Order

Jade files are concatenated in no particular order as the Jade include system
is used for explicit ordering.


## Automatic rewriting for Jade

duplo does not and cannot peek into Jade's include system. However, it does
automatically expand paths in include statements to make the inclusion process
easier. Take this example:

```jade
include index.jade
include menu/index.jade
include pixbi-helper/index.jade
```

In the absence of a Component repo string (i.e. `<user>-<repo>`), the path is
assumed to be pointing to a file under the `modules` directory in the current
repo. With a component repo string, it is assumed to also be pointing to a file
under the `modules` directory, but in the corresponding component's repo.

The above is effectively rewritten into these paths, relative to the top-level
repo's directory.

```jade
include app/modules/index.jade
include app/modules/menu/index.jade
include components/pixbi-helper/app/modules/index.jade
```


## A note on the `modules` directory

By now, it should be obvious that there are really two "modes" for any duplo
repo: an application mode and a library mode. In application mode, duplo acts
as the top-level program, including other duplo repos via Component as
libraries. In this scenario, `app/index.js` and `app/index.jade` are included
into the build. Contrast this to the library mode, where only those in the
"second" level (e.g. `modules/`, `assets/`, `styl/`) are included into the
build.


## Component Versions

Each component's version is recorded in the `DUPLO_VERSIONS` global variable,
in the form similar to:

```json
{
  "pixbi-main": "4.1.9",
  "pixbi-launcher": "0.1.4"
}
```


## Dependency Selection

Some cases require the repo to be polymorphic in the sense that we could
generate different forms of the same codebase. For example, you may need to
build the repo in an embeddable form which would exclude certain dependencies
that are required in its standalone form.

In this case you would include a `modes` attribute in the `component.json`
manifest file. The attribute would contain an `embeddable` and a `standalone`
attributes, each of which would then contain an array of dependencies as
specified in the `dependencies` attribute to include.

Running duplo with the environment variable `DUPLO_MODE` set to `embeddable`
would build with the dependencies specified under `embeddable` while setting
`MODE` to `standalone` would do the same with those specified under the
`standalone` attribute. Otherwise duplo would just build with all dependencies.

Note that dependency selection applies at the dependency level but not at the
file level within the components.

Also note that duplo caches between builds. When you switch dependency
selection, remember to `duplo clean` your repo first.

Putting it all together, an example of a `component.json`:

```json
{
  "dependencies": {
    "pixbi/sdk": "1.1.1",
    "pixbi/embeddable": "2.2.2",
    "pixbi/standalone": "3.3.3"
  },
  "modes": {
    "embeddable": [
      "pixbi/standalone"
    ],
    "standalone": [
      "pixbi/embeddable"
    ]
  }
}
```


## Copyright and License

Code and documentation copyright 2014 Pixbi. Code released under the MIT
license. Docs released under Creative Commons.
