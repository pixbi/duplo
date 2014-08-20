NODE_ENV = process.env.NODE_ENV or 'prod'

module.exports = (grunt) ->
  ####
  ## Auxiliary constants and functions
  ####

  # Callback is called if the provided file path points to a file that exists
  whenExists = (path, done) ->
    done(path) if grunt.file.exists(path)

  # All the manifest files of this repo and its dependencies
  manifest = null
  manifests = do ->
    whenExists './component.json', (path) ->
      manifest = grunt.file.readJSON(path)
      deps = Object.keys(manifest.dependencies or {})
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

    'copy:defaultTemplate'
    'copy:defaultStyle'
    'copy:assets'
    'copy:dev'

    'compile:js'
    'compile:stylus'
    'compile:jade'
    'inject:version'

    'concat:js'
    'concat:css'
    'concat:html'
    'dom_munger:link'

    'clean:staging'
  ]

  # Template-related
  templateTasks = [
    'jade'
  ]

  # Style-related
  styleTasks = [
    'stylus'
    'autoprefixer'
    'cssshrink'
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
      defaultTemplate:
        src: "#{rootPath}/assets/index.html"
        dest: 'public/index.html'
      defaultStyle:
        src: "#{rootPath}/assets/index.css"
        dest: 'staging/index.css'

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
          'components/**/staging/index.css'
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
          "echo '#{content}' > staging/version.js"

      make:
        command: (task, path = '.') ->
          "BASE=#{path} make #{task}"

    dom_munger:
      link:
        src: 'public/index.html'
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
      default:
        options:
          paths: 'app/styl/variables.styl'
          import: 'variables'
        files:
          'staging/index.css': [
            'app/styl/keyframes.styl'
            'app/styl/fonts.styl'
            'app/styl/reset.styl'
            'app/styl/main.styl'
            'app/modules/**/index.styl'
            'app/modules/**/*.styl'
          ]

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
        basedir: 'app/'
      default:
        files:
          'staging/index.html': 'app/index.jade'

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
    # First compile this repo
    switch type
      when 'js'
        if NODE_ENV isnt 'dev'
          grunt.task.run(scriptTasks)
      when 'stylus'
        grunt.task.run(styleTasks)
      when 'jade'
        grunt.task.run(templateTasks)

    # Then compile its dependencies
    runOnDeps('build')

  grunt.registerTask 'check', ['closure-compiler']

  grunt.registerTask 'dev', ['build', 'connect', 'watch']

  grunt.registerTask 'build', compileTasks

  grunt.registerTask 'tag', ->
    grunt.task.run("shell:tag:#{manifest.version}")

  grunt.registerTask 'inject', (type) ->
    switch type
      when 'version'
        appName = manifest.name
        version = manifest.version
        task = "shell:writeVersion:#{appName}:#{version}"
        grunt.task.run(task)
