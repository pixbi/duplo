# duplo

An opinionated but intuitive build tool for web application


## Installation

    $ npm install -g duplo


## Usage

* `duplo version` displays the version for this duplo installation.
* `duplo new <user> <repo>` scaffolds a new duplo repo with a GitHub user.
  handle and a repo name. The repo is assumed to have been created.
* `duplo build` or simply `duplo` starts a server and re-compiles on file
  change.
* `duplo patch` bumps the patch version.
* `duplo minor` bumps the minor version.
* `duplo major` bumps the major version.


## Guiding Principle

This is a build tool, not an application framework. It simply compiles and
builds your codebase for you and does not inject a runtime into your
application.

However, it does have a list of requirements and assumptions, specifically:

* [GitHub](http://github.com/) and by extension [git](http://git-scm.com/) for
  source code management
* [Heroku](https://www.heroku.com/) for application deployment
* [CircleCI](https://circleci.com/) for continuous integration
* [Jade](http://jade-lang.com/) over HTML
* [Stylus](http://learnboost.github.io/stylus/) over CSS

The idea is to manage and deploy your code exclusively with GitHub and have
CircleCI deals with deployment for you. However, duplo is a build tool; the
exact structure of your application is not what it concerns itself with. This
means that all scripts are dumped into one single file, and so are the
stylesheets and the markup.


## File Structure

    app/            --> Application code
    app/index.jade  --> Entry point for templates. Only this file is compiled.
                        Use Jade's include system to pull in other templates.
    app/index.json  --> Optional parameter object made available as `APP`
    app/index.js    --> Application entry point
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


## Environment

duplo injects is the `DUPLO_ENV` global variable. It contains a string that is
carried over as-is from the environment variable `DUPLO_ENV` passed to duplo.
There is no default value.


## Entry Point

Every application has a main entry point. In a duplo application, it is
`app/index.js`. Each repo may contain its own `app/index.js` but only the repo
in which duplo is run is its `app/index.js` executed. `app/index.js` files of
other repos that are pulled into the top-level repo via Component.IO are
ignored.


## JavaScript Concatenation Order

JavaScript files are not concatenated in any particular order. It is good
practice to just initialize functions that would be called later on by the main
repo whose `app/index.js` is called.


## CSS/Stylus Concatenation Order

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

The concatenation order between different dependencies is unspecified.


## HTML/Jade Concatenation Order

Jade files are concatenated in no particular order.


## Dependency Selection

Some cases require the repo to be polymorphic in the sense that we could
generate different forms of the same codebase. For example, you may need to
build the repo in an embeddable form which would exclude certain dependencies
that are required in its standalone form.

In this case you would include an `modes` attribute in the `component.json`
manifest file. The attribute would contain an `embeddable` and a `standalone`
attributes, each of which would then contain an array of dependencies as
specified in the `dependencies` attribute to include.

Running duplo with the environment variable `MODE` set to `embeddable` would
build with the specified dependencies under `embeddable` while setting `MODE`
to `standalone` would do the same with those specified under the `standalone`
attribute. Otherwise duplo would build with all dependencies.

Note that dependency selection applies at the dependency level but not at the
file level within the components.

An example of a `component.json`:

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
