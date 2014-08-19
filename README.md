# Pixbi Gruntfile

This repo basically contains the Gruntfile that is used across all frontend
projects. The project using this repo must follow a set of conventions outlined
below.


## Installation

1. Install via npm:

   `$ npm install --save-dev https://github.com/pixbi/grunt/tarball/master`

2. Make sure you copy the `Makefile` from this repo:

   `$ cp node_modules/pixbi-grunt/Makefile .`

3. Copy over the `.gitignore` file as well:

   `$ cp node_modules/pixbi-grunt/gitignore ./.gitignore`


## Usage

Four commands:

* `make dev` starts a local server and re-compiles on file change
* `make patch` builds the project and bump the patch version
* `make minor` builds the project and bump the minor version
* `make major` builds the project and bump the major version

Note that building the project performs these steps:

1. Checkout the `develop` branch
2. Build the project to the `build/` directory
3. Bump the respective version
4. Commit to git
5. Checkout the `master` branch and merge `develop` into `master`
6. Apply the new version as a new git tag
7. Checkout the `develop` branch again


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
