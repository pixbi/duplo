# duplo

A opinionated, framework-less build tool for web applications


## Installation

    $ npm install -g duplo


## Usage

* `duplo version` displays the version for this duplo installation.
* `duplo help` displays all commands.
* `duplo new <user> <repo>` scaffolds a new duplo repo with a GitHub user
  handle and a repo name. The repo is assumed to already exist.
* `duplo build` builds the project. Pass in `dev` to `DUPLO_ENV` environment
  variable to build in development mode.
* `duplo serve` runs `duplo build`, starts a server, and rebuilds on file
  change.
* `duplo dev`: short-hand for `DUPLO_ENV=dev duplo serve`.
* `duplo staging`: short-hand for `DUPLO_ENV=staging duplo serve`.
* `duplo live`: short-hand for `DUPLO_ENV=live duplo serve`.
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

    app/index.jade  --> Entry point for templates. Only this file is compiled.
                        Use Jade's include system to pull in other templates.
    app/index.js    --> Application entry point. Only the top-level
                        application's `index.js is included and run. Its
                        dependencies' are ignored.
    app/assets/     --> Asset files are copied as-is to build's top-level
                        directory
    app/styl/       --> Application stylesheets
    app/modules/    --> Application code
    components/     --> Other repos imported via Component.IO
    component.json  --> The Component.IO manifest
    dev/            --> Files here are copied as-is to project root when
                        building in development mode
    public/         --> Built files when developing. Not committed to source
    test/           --> Test files go here


## Development

During development, everything in the `dev/` directory is copied over as-is *at
the end* of the build process. This means that files in the directory would
replace whatever that has been built at their respective locations. So the
`dev/index.html` would need to reference the script and the tag manually, e.g.

    <html>
      <head>
        <link rel="stylesheet" href="style.css"/>
      </head>
      <body>
        <script src="script.js"></script>
      </body>
    </html>


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


## Application Parameterization

If you need some build-time customization of the app, such as customizing each
build with a JSON object of unique IDs and metadata, you can pass any string
as the environment variable `DUPLO_IN`. The string is then turned into a
JavaScript string and stored into a global variable.

For example, say you need to pass in a random ID for each build, you would
invoke:

```sh
$ echo "{\"id\":\""$randomId"\"}" | duplo
```

Then in `app/index.js`:

```js
var randomId = JSON.parse(DUPLO_IN).id;
```

Note that all newline characters are removed before the string is wrapped into
a JavaScript string.


## JavaScript Concatenation Order

JavaScript files are not concatenated in any particular order. It is good
practice to just define functions that would be called later on by the main
repo when its `app/index.js` is called.


## CSS/Stylus Concatenation Order

Unlike script files, where you place your CSS files within `app/` is
significant. Stylus files will be concatenated in this order:

    app/styl/variables.styl  --> An optional variable file that gets injected
                                 into every Stylus file
    app/styl/keyframes.styl  --> Keyframes
    app/styl/fonts.styl      --> Font declarations
    app/styl/reset.styl      --> Resetting existing CSS in the target
                                 environment
    app/styl/main.styl       --> Application CSS that goes before any module
                                 CSS
    app/modules/*/index.styl --> CSS relevant to specific modules

There is no particular concatenation order between different dependencies.


## HTML/Jade Concatenation Order

Jade files are concatenated in no particular order as the Jade include system
is used for explicit ordering. Note that any Jade files within the `app/`
directory are not included. They must be within the `app/modules/` directory.


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
`standalone` attribute. Otherwise duplo would build with all dependencies.

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
