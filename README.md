# Pixbi Gruntfile

This repo basically contains the Gruntfile that is used across all frontend
projects. The project using this repo must follow a set of conventions outlined
below.


## Installation

Run the following commands.

```
$ npm install -g grunt-cli
$ npm install --save-dev https://github.com/pixbi/grunt/tarball/master
$ cp node_modules/pixbi-grunt/Makefile .
$ cp node_modules/pixbi-grunt/gitignore ./.gitignore
```

Basically:

1. Install Grunt.js
2. Install via npm
3. Copy the `Makefile` from this repo
4. Copy over the `.gitignore` file as well

The reason behind using Makefile to run a Gruntfile is for abstraction and
centralized management of the build tool (e.g. versioning).


## Usage

Four commands:

* `make dev` starts a local server and re-compiles on file change
* `make patch` builds the project and bump the patch version
* `make minor` builds the project and bump the minor version
* `make major` builds the project and bump the major version

Note that building the project performs these steps:

1. Stash changes to git to avoid data loss (you should of course make sure
   there is no uncommitted code as well)
2. Checkout the `develop` branch
3. Build the project to the `build/` directory
4. Bump the respective version
5. Commit to git
6. Checkout the `master` branch and merge `develop` into `master`
7. Apply the new version as a new git tag
8. Checkout the `develop` branch again


## File Structure

There are some required directories:

    app/          --> Application code
    app/assets/   --> Asset files are copied as-is to build's top-level
                      directory
    app/modules/  --> Module within the application that are ordered AFTER code
                      in the top-level `app/` directory when building
    build/        --> When bumping version, a new build is placed here
    components/   --> Other repos imported via Component.IO
    dev/          --> Any code necessary to run the applcation in dev mode
    node_modules/ --> Contains this repo
    public/       --> Built files when developing. This is NOT committed to
                      source

Note that test files reside alongside with the application code in question and
has an extension of `.spec.js`.
