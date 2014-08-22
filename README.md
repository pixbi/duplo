# Duplo

    Intuitive, simple building blocks for building composable, fully managed
    web applications


## Installation

    $ npm install -g duplo

That's it!


## Usage

* `duplo dev` starts a local server and re-compiles on file change
* `duplo build` runs a build. This could be used for checking the code against
  Closure Compiler
* `duplo patch` builds the project and bump the patch version
* `duplo minor` builds the project and bump the minor version
* `duplo major` builds the project and bump the major version

### Compiling

Compiling the project performs these steps:

1. Copy files in `app/assets/` to `public/`
2. Copy files in `dev/` to `public/`
3. `public/index.html` is created if it doesn't already exist
4. Compile all Stylus files (order specified [below](#cssstylus-order)) under
   `app/` and concatenate into one CSS file as `public/index.css`
5. Concatenate all JavaScript under `app/` into one JS file as
   `public/index.js`
6. Compile all Jade files under `app/` into one HTML file and inject into the
   end of `body` in `public/index.html`
7. Write into `public/index.html` tags to include `style.css` and `script.js`

Note that while compiling the builder creates a temporary `tmp/` directory.

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


## Technologies

* [Jade](http://jade-lang.com/) over HTML
* [Stylus](http://learnboost.github.io/stylus/) over CSS
* JavaScript


## File Structure

There are some required directories:

    app/            --> Application code
    app/index.html  --> An optional special HTML file into which the builder
                        injects the script tag, the style tag, and the markup
                        when built
    app/index.jade  --> Entry point for templates. Only this file is compiled.
                        Use Jade's include system to pull in other templates.
    app/params.json --> Optional parameter object to be made available as
                        `module.params`
    app/assets/     --> Asset files are copied as-is to build's top-level
                        directory
    app/styl/       --> Any application top-level style (see below for details)
    app/modules/    --> Module within the application that are ordered AFTER
                        code in the top-level `app/` directory when building
    components/     --> Other repos imported via Component.IO
    dev/            --> Any code necessary to run the application in dev mode
    node_modules/   --> Contains this repo
    public/         --> Built files when developing. This is NOT committed to
                        source
    test/           --> Test files go here and should have an extension of
                        `.spec.js`


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

The output file exposes the mode via the `module.mode` attribute. When in
development, `module.mode === 'dev'` should be `true`.

NOTE: because of the builder recursively building dependencies, it does not
watch for changes in `components/` directory. If you update a repo's
dependencies, you need to restart Grunt.js.


## Application Parameters

Since the bootloader accepts a `params` object for `module.init()`, you may
specify an optional `params.json`, the content of which would be injected as
`module.params`. For instance, with a `params.json` of:

```json
{
  "config": {
    "kickass": true
  }
}
```

`public/script.js` would look something like:

```js
...
module.mode = "dev";
module.params = {
  "config": {
    "kickass": true
  }
};
```

And we then may call it in `index.html` like this:

```html
<body>
  <script>
    document.addEventListener('DOMContentLoaded', function () {
      module.init(module.params);
    });
  </script>
</body>
```

The benefit of this is that we could place a `params.json` in `dev/` for dev
mode and one in `app/` for production and have a complete isolation between code
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
