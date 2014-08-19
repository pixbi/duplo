# Pixbi Build Tool

This repo basically contains the build tool that is used across all frontend
projects. The project using this repo must follow a set of conventions outlined
below.


## Installation

Run the following commands.

```
$ npm install -g grunt-cli
$ npm install --save-dev https://github.com/pixbi/build/tarball/master
$ cp node_modules/pixbi-build/Makefile .
$ cp node_modules/pixbi-build/gitignore ./.gitignore
```

Basically:

1. Install Grunt.js
2. Install via npm
3. Copy the `Makefile` from this repo
4. Copy over the `.gitignore` file as well

The reason behind using Makefile to run a Gruntfile is for abstraction and
centralized management of the build tool (e.g. for versioning).


## Usage

Four commands:

* `make dev` starts a local server and re-compiles on file change
* `make check` runs the Closure Compiler by itself for error checking
* `make patch` builds the project and bump the patch version
* `make minor` builds the project and bump the minor version
* `make major` builds the project and bump the major version

Compiling the project performs these steps:

1. Copy files in `app/assets/` to `public/`
2. Copy files in `dev/` to `public/`
3. `public/index.html` is created if it doesn't already exist
4. Compile all Stylus files (order specified [below](#css-order)) under `app/`
   and concatenate into one CSS file as `public/style.css`
5. Concatenate all JavaScript under `app/` into one JS file as
   `public/script.js`
6. Compile all Jade files under `app/` into one HTML file and inject into the
   end of `body` in `public/index.html`
7. Write into `public/index.html` tags to include `style.css` and `script.js`

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


## Technologies

* [Jade](http://jade-lang.com/) over HTML
* [Stylus](http://learnboost.github.io/stylus/) over CSS
* JavaScript


## File Structure

There are some required directories:

    app/          --> Application code
    app/assets/   --> Asset files are copied as-is to build's top-level
                      directory
    app/styl/     --> Any application top-level style (see below for details)
    app/modules/  --> Module within the application that are ordered AFTER code
                      in the top-level `app/` directory when building
    components/   --> Other repos imported via Component.IO
    dev/          --> Any code necessary to run the application in dev mode
    node_modules/ --> Contains this repo
    public/       --> Built files when developing. This is NOT committed to
                      source

Note that test files reside alongside with the corresponding application code
and has an extension of `.spec.js`.


## CSS Order

Where you place your CSS files within `app/` is significant. Stylus files will be concatenated in this order:

    app/styl/keyframes.styl   --> Keyframes
    app/styl/fonts.styl       --> Font declarations
    app/styl/reset.styl       --> Resetting existing CSS in the target
                                  environment
    app/styl/main.styl        --> Application CSS that goes before any module
                                  CSS
    app/modules/**/index.styl --> CSS relevant to specific modules
    app/styl/mobile.styl      --> Mobile-specific CSS
