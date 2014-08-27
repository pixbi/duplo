NODE_ENV = process.env.NODE_ENV or 'dev'
DUPLO = process.cwd()
CWD = process.env.CWD or '~'

module.exports = (grunt) ->
  require('load-grunt-tasks') grunt,
    config: "#{DUPLO}/package.json"

  process.chdir(CWD)

  ####
  ## Auxiliary constants and functions
  ####

  styleVariableFile = "#{CWD}/app/styl/variables.styl"

  # Callback is called if the provided file path points to a file that exists
  whenExists = (path, done) ->
    done(path) if grunt.file.exists(path)

  # Get the most up-to-date version
  getVersion = ->
    grunt.file.readJSON('./component.json').version

  # All the manifest files of this repo and its dependencies
  thisManifest = null
  manifests = do ->
    whenExists './component.json', (path) ->
      thisManifest = grunt.file.readJSON(path)
      deps = Object.keys(thisManifest.dependencies or {})
      output = []

      # Find this repo's dependency manifests
      for dep in deps
        dep = dep.replace('/', '-')
        whenExists "./components/#{dep}/component.json", (path) ->
          m = grunt.file.readJSON(path)
          m.path = "./components/#{dep}/"
          output.push(m)

      output

  # Run a task over the repo's dependencies
  runOnDeps = (task) ->
    for manifest in manifests
      path = manifest.path or process.cwd()

      whenExists path, (path) ->
        grunt.task.run("shell:duplo:#{task}:#{path}")

  # Find root
  findRoot = (pathArray) ->
    pathArray ?= process.cwd().split('/')

    if pathArray.length <= 0
      ['/']
    else
      target = pathArray.join('/') + '/node_modules/pixbi-build'

      if grunt.file.exists(target)
        target.split('/')
      else
        pathArray.pop()
        findRoot pathArray

  rootPathArray = do findRoot
  rootPath = rootPathArray.join('/')

  # Chain an arra of commands into one string
  chainCommands = (commands...) ->
    commands.map((command) ->
      command.join(';')
    ).join(';')


  ####
  ## Tasks
  ####

  compileTasks = [
    'clean:all'

    'copyComponentAssets'
    'copy:assets'
    # Copy over the app's params first, then the dev version, which takes
    # precedence
    'copy:paramsApp'
    'copy:paramsDev'
    'copy:dev'

    'compile:js'
    'compile:stylus'
    'compile:jade'
    'compile:deps'
    'inject:version'

    'concat:js'
    'concat:css'
    'concat:html'
    'inject:mode'
    'inject:index'
    'inject:params'
    'concat:params'

    'optimize'
    'link'

    'clean:build'
  ]

  # Template-related
  templateTasks = [
    'jade'
  ]

  # Stylus files follow an order
  styleFiles = [
    'app/styl/keyframes.styl'
    'app/styl/fonts.styl'
    'app/styl/reset.styl'
    'app/styl/main.styl'
    'app/modules/**/index.styl'
    'app/modules/**/*.styl'
  ]

  # Script optimization tasks
  optimizeScriptTasks = [
    'uglify'
  ]


  grunt.initConfig

    ## General

    watch:
      default:
        files: [
          'app/**/*'
          'components/**/app/**/*'
          'dev/**/*'
        ]
        tasks: compileTasks
        options:
          livereload: true

    clean:
      all: [
        'tmp'
        'public'
      ]

      build: [
        'tmp'
        'public/**/*.json'
        'public/template.html'
      ]

    copy:
      assets:
        expand: true
        cwd: 'app/assets/'
        src: '**/*'
        dest: 'public/'

      dev:
        expand: true
        cwd: 'dev/'
        src: [
          '**/*'
          # Params are managed separately
          '!params.json'
        ]
        dest: 'public/'

      index:
        src: 'app/index.html'
        dest: 'public/index.html'

      js:
        expand: true
        cwd: 'app/'
        src: '**/*.js'
        dest: 'tmp/'

      paramsApp:
        src: 'app/params.json'
        dest: 'tmp/params.json'
      paramsDev:
        src: 'dev/params.json'
        dest: 'tmp/params.json'

    concat:
      js:
        src: [
          # Bootloader needs to be built first
          'components/pixbi-bootloader/public/script.js'
          'components/**/public/script.js'
          'tmp/**/*.js'
        ]
        dest: 'tmp/script.js'

      css:
        src: [
          'components/**/public/style.css'
          'public/style.css'
        ]
        dest: 'public/style.css'

      html:
        src: [
          'components/**/public/template.html'
          'public/template.html'
        ]
        dest: 'public/template.html'

      params:
        src: [
          'tmp/script.js'
          'tmp/params.js'
        ]
        dest: 'public/script.js'

    connect:
      server:
        options:
          port: 8888
          hostname: '*'
          base: 'public'

    bump:
      options:
        files: ['component.json']
        updateConfigs: []
        commit: false
        createTag: false
        push: false

    shell:
      writeVersion:
        command: (appName, version) ->
          appName = appName.replace(/-/g, '_')
          content = "module.#{appName}.version = \"#{version}\";"
          "mkdir -p tmp; echo '#{content}' >> tmp/version.js"

      writeMode:
        command: (mode) ->
          "echo 'module.mode = \"#{mode}\";' >> public/script.js"

      precommit:
        command: ->
          chainCommands [
            'git stash'
            'git checkout develop'
          ]

      commit:
        command: ->
          chainCommands [
            # Commit
            'git add component.json'
            "git commit -m 'Bump version'"
            # Merge into master
            'git checkout master'
            # Always force the new changes
            'git merge develop -X theirs'
            # Apply tag
            "git tag #{getVersion()}"
            ## Sync with Github
            'git push origin develop:develop'
            'git push origin master:master'
            'git push origin --tags'
            # Go back to develop
            'git checkout develop'
          ]

      duplo:
        command: (task, path) ->
          path = "#{process.cwd()}/#{path}"
          "duplo #{task} #{path}"

    dom_munger:
      link:
        src: 'public/index.html'
        dest: 'public/index.html'
        options:
          append: [
            selector: 'head'
            html: '<link rel="stylesheet" type="text/css" href="style.css"/>'
          ,
            selector: 'head'
            html: '<script type="text/javascript" src="script.js" defer="defer"></script>'
          ]

          callback: ($) ->
            $('body').prepend(grunt.file.read('public/template.html'))

            # Defer cleaning because stupid jQuery `$.append` doesn't provide a callback
            setTimeout ->
              grunt.task.run('clean:build')
            , 100


    ## Style-specific

    stylus:
      options:
        # Let CSSShrink do the compression when building for production
        compress: false

      withVariables:
        options:
          paths: [styleVariableFile]
          import: ['variables']
        files:
          'public/style.css': styleFiles

      noVariables:
        files:
          'public/style.css': styleFiles

    autoprefixer:
      default:
        src: 'public/style.css'
        dest: 'public/style.css'
        options:
          browsers: ['last 2 Chrome versions', 'last 2 iOS versions', 'ie 10']

    cssshrink:
      default:
        files:
          'public/style.css': 'public/style.css'

    ## Template-specific

    jade:
      options:
        # Let downstream build step handles compression when building for
        # production
        pretty: true
      default:
        files:
          # Use the Jade include system, so only include `index.jade` here
          'public/template.html': 'app/index.jade'

    ## Script-specific

    'closure-compiler':
      default:
        closurePath: __dirname
        js: 'public/script.js'
        jsOutputFile: 'public/script.js'
        options:
          compilation_level: 'ADVANCED_OPTIMIZATIONS'
          language_in: 'ECMASCRIPT5_STRICT'
          warning_level: 'verbose'
          summary_detail_level: 3

    uglify:
      default:
        files:
          src: 'public/script.js'
          dest: 'public/script.js'


  ####
  ## Custom tasks
  ####

  grunt.registerTask 'dev', compileTasks.concat ['connect', 'watch']

  grunt.registerTask 'build', compileTasks

  grunt.registerTask 'link', 'dom_munger:link'

  grunt.registerTask 'release', (level) ->
    grunt.task.run [
      'shell:precommit'
      "bump:#{level}"
      'updateComponent'
      'shell:commit'
    ]

  grunt.registerTask 'optimize', ->
    # TODO: turn on after our refactoring
    if NODE_ENV isnt 'dev'
      grunt.task.run(optimizeScriptTasks)

  grunt.registerTask 'updateComponent', ->
    manifest = grunt.file.readJSON('component.json')
    manifest.scripts = grunt.file.expand('app/**/*.js')
    manifest.styles = grunt.file.expand('app/**/*.styl')
    manifest.templates = grunt.file.expand('app/**/*.jade')
    manifest.images = grunt.file.expand('app/assets/images/**/*.*')
    manifest.fonts = grunt.file.expand('app/assets/fonts/**/*.*')

    content = JSON.stringify(manifest, null, '  ')
    grunt.file.write('component.json', content)

  grunt.registerTask 'inject', (type) ->
    switch type
      when 'version'
        appName = thisManifest.name
        version = thisManifest.version
        task = "shell:writeVersion:#{appName}:#{version}"
        grunt.task.run(task)

      when 'mode'
        grunt.task.run("shell:writeMode:#{NODE_ENV}")

      when 'index'
        grunt.task.run('copy:index')

        if NODE_ENV is 'dev'
          grunt.task.run('copy:dev')

      when 'params'
        whenExists 'tmp/params.json', (path) ->
          content =
            'module.params = ' +
            JSON.stringify(grunt.file.readJSON(path), null, '  ') +
            ';'
          grunt.file.write('tmp/params.js', content)

  grunt.registerTask 'compile', (type) ->
    switch type
      when 'js'
        grunt.task.run('copy:js')

      when 'stylus'
        # Compile with or without variable injection
        if grunt.file.exists(styleVariableFile)
          grunt.task.run [
            'stylus:withVariables'
            'runAutoprefixer'
            'runCssShrink'
          ]
        else
          grunt.task.run [
            'stylus:noVariables'
            'runAutoprefixer'
            'runCssShrink'
          ]

      when 'jade'
        grunt.task.run(templateTasks)

      # Compile dependencies
      when 'deps'
        runOnDeps('build')

  grunt.registerTask 'runAutoprefixer', ->
    if grunt.file.exists('public/style.css')
      grunt.task.run 'autoprefixer'

  # Optimize only when we're in production and if there are stylesheets
  grunt.registerTask 'runCssShrink', ->
    if NODE_ENV isnt 'dev' and grunt.task.exists('public/style.css')
      grunt.task.run('cssshrink')

  # We need to manually handle components as the `cwd` path is dynamic
  # depending on the assets
  grunt.registerTask 'copyComponentAssets', ->
    assets = grunt.file.expand 'components/*/app/assets/**/*'
    pattern = /^components\/[^\/]+\/app\/assets\//

    for asset in assets
      assetRelPath = asset.replace(pattern, '')

      if grunt.file.isFile(asset)
        grunt.file.copy asset, "public/#{assetRelPath}"
