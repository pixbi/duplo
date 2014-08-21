NODE_ENV = process.env.NODE_ENV or 'prod'

module.exports = (grunt) ->
  ####
  ## Auxiliary constants and functions
  ####

  styleVariableFile = 'app/styl/variables.styl'

  # Callback is called if the provided file path points to a file that exists
  whenExists = (path, done) ->
    done(path) if grunt.file.exists(path)

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
      path = manifest.path or '.'

      whenExists path, (path) ->
        grunt.task.run("shell:make:#{task}:#{path}")

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

  # Load Grunt plugins from `pixbi-build`
  loadPlugins = (plugins) ->
    # Special handling with current directory for Grunt
    cwd = process.cwd()
    path = rootPathArray.join('/')

    whenExists path, (path) ->
      process.chdir(path)
      for plugin in plugins
        grunt.task.loadNpmTasks plugin
      process.chdir(cwd)

  # Grunt plugins need to be loaded manually because of the weird setup that we
  # have (i.e. grunt plugins not being local to the repo)
  do ->
    whenExists "#{__dirname}/package.json", (path) ->
      manifest = grunt.file.readJSON(path)
      deps = manifest.dependencies
      delete deps.grunt
      loadPlugins Object.keys(deps)


  ####
  ## Tasks
  ####

  # Always call explicitly with parameter to allow easy extension in the future
  compileTasks = [
    'clean:public'
    'clean:staging'

    'copy:assets'
    'copy:dev'

    'compile:js'
    'compile:stylus'
    'compile:jade'
    'compile:deps'
    'inject:version'

    'concat:js'
    'concat:css'
    'concat:html'
    'dom_munger:link'
  ]

  # Template-related
  templateTasks = [
    'jade'
  ]

  # Style-related
  styleTasks = [
    'autoprefixer'
    'cssshrink'
  ]

  styleOrder = [
    'app/styl/keyframes.styl'
    'app/styl/fonts.styl'
    'app/styl/reset.styl'
    'app/styl/main.styl'
    'app/modules/**/index.styl'
    'app/modules/**/*.styl'
  ]

  # Stylus files follow an order
  styleFiles = [
  ]

  # Script-related
  scriptTasks = [
    'closure-compiler'
    'uglify'
  ]


  grunt.initConfig

    ## General

    watch:
      default:
        files: [
          'app/**/*'
          'components/**/*'
          'dev/**/*'
        ]
        tasks: 'build'
        options:
          livereload: true

    clean:
      public:
        src: 'public'
      staging:
        src: 'staging'

    copy:
      assets:
        expand: true
        cwd: 'app/assets/'
        src: '**/*'
        dest: 'public/'

      dev:
        expand: true
        cwd: 'dev/'
        src: '**/*'
        dest: 'public/'

      js:
        expand: true
        cwd: 'app/'
        src: '**/*.js'
        dest: 'staging/'

    concat:
      js:
        src: [
          'staging/**/*.js'
          'components/**/staging/*.js'
        ]
        dest: 'public/index.js'
      css:
        src: [
          'staging/index.css'
          'components/**/staging/*.css'
        ]
        dest: 'public/index.css'
      html:
        src: [
          'staging/index.html'
          'components/**/staging/index.html'
        ]
        dest: 'staging/index.html'

    connect:
      server:
        options:
          port: 8000
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
      tag:
        command: (version) ->
          "git tag #{version}"

      writeVersion:
        command: (appName, version) ->
          content = "module.#{appName}.version = '#{version}';"
          content = content.replace(/[0-9a-zA-Z]+-/, '')
          content = content.replace(/'/g, "''")
          "mkdir -p staging; echo '#{content}' >> staging/version.js"

      make:
        command: (task, path = '.') ->
          "BASE=#{path} make #{task}"

    dom_munger:
      link:
        src: 'app/index.html'
        dest: 'public/index.html'
        options:
          append: [
            selector: 'head'
            html: '<link rel="stylesheet" type="text/css" href="index.css"/>'
          ,
            selector: 'body'
            html: '<script type="text/javascript" src="index.js"></script>'
          ]
          callback: ($) ->
            template = grunt.file.read('staging/index.html')
            $('body').html(template)


    ## Style-specific

    stylus:
      options:
        # Let CSSShrink do the compression when building for production
        compress: false

      withVariables:
        options:
          paths: styleVariableFile
          import: 'variables'
        files:
          'staging/index.css': styleOrder

      noVariables:
        files:
          'staging/index.css': styleOrder

    autoprefixer:
      default:
        src: 'staging/index.css'
        dest: 'public/index.css'
        options:
          browsers: ['last 2 Chrome versions', 'last 2 iOS versions', 'ie 10']

    cssshrink:
      default:
        files:
          'public/index.css': 'public/index.css'

    ## Template-specific

    jade:
      options:
        # Let downstream build step handles compression when building for
        # production
        pretty: true
      default:
        files:
          # Use the Jade include system, so only include `index.jade` here
          'staging/index.html': 'app/jade/index.jade'

    ## Script-specific

    uglify:
      default:
        files:
          'public/index.js': 'public/index.js'

    'closure-compiler':
      default:
        closurePath: "#{__dirname}/compiler.jar"
        js: 'public/index.js'
        jsOutputFile: 'public/index.js'
        options:
          compilation_level: 'ADVANCED_OPTIMIZATIONS'
          language_in: 'ECMASCRIPT5_STRICT'
          warning_level: 'verbose'
          summary_detail_level: 3


  ####
  ## Custom tasks
  ####

  grunt.registerTask 'compile', (type) ->
    switch type
      when 'js'
        grunt.task.run('copy:js')

        # Optimize
        if NODE_ENV isnt 'dev'
          grunt.task.run(scriptTasks)

      when 'stylus'
        # Compile with or without variable injection
        if grunt.task.exists(styleVariableFile)
          grunt.task.run('stylus:withVariables')
        else
          grunt.task.run('stylus:noVariables')

        # Optimize only when we're in production and if there are stylesheets
        if NODE_ENV isnt 'dev' and
           grunt.file.expand('app/**/*.styl').length > 0
          grunt.task.run(styleTasks)

      when 'jade'
        grunt.task.run(templateTasks)

      # Compile dependencies
      when 'deps'
        runOnDeps('build')

  grunt.registerTask 'check', ['closure-compiler']

  grunt.registerTask 'dev', ['build', 'connect', 'watch']

  grunt.registerTask 'build', compileTasks

  grunt.registerTask 'tag', ->
    grunt.task.run("shell:tag:#{thisManifest.version}")

  grunt.registerTask 'inject', (type) ->
    switch type
      when 'version'
        appName = thisManifest.name
        version = thisManifest.version
        task = "shell:writeVersion:#{appName}:#{version}"
        grunt.task.run(task)
