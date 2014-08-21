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
    'clean:tmp'

    'copy:assets'

    'compile:js'
    'compile:stylus'
    'compile:jade'
    'compile:deps'
    'inject:version'

    'concat:js'
    'concat:css'
    'concat:html'

    'optimize'
    'link'

    'clean:tmp'
  ]

  devTasks = compileTasks.concat [
    'copy:dev'
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
    'closure-compiler'
    'copy:jsmin'
    'clean:jsmin'
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
        tasks: devTasks
        options:
          livereload: true

    clean:
      public:
        src: 'public'

      tmp:
        src: 'tmp'

      jsmin:
        src: 'public/script.min.js'

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
        dest: 'tmp/'

      jsmin:
        src: 'public/script.min.js'
        dest: 'public/script.js'

    concat:
      js:
        src: [
          # Bootloader needs to be built first
          'components/pixbi-bootloader/public/script.js'
          'components/**/public/script.js'
          'tmp/**/*.js'
        ]
        dest: 'public/script.js'
      css:
        src: [
          'components/**/public/style.css'
          'tmp/style.css'
        ]
        dest: 'public/style.css'
      html:
        src: [
          'components/**/public/template.html'
          'tmp/template.html'
        ]
        dest: 'public/template.html'

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
          appName = appName.replace(/-/g, '_')
          content = "module.#{appName}.version = \"#{version}\";"
          "mkdir -p tmp; echo '#{content}' >> tmp/version.js"

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
            html: '<link rel="stylesheet" type="text/css" href="style.css"/>'
          ,
            selector: 'body'
            html: '<script type="text/javascript" src="script.js"></script>'
          ]
          callback: ($) ->
            template = grunt.file.read('public/template.html')
            $('body').prepend(template)


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
          'tmp/style.css': styleFiles

      noVariables:
        files:
          'tmp/style.css': styleFiles

    autoprefixer:
      default:
        src: 'tmp/style.css'
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
          'tmp/template.html': 'app/jade/index.jade'

    ## Script-specific

    'closure-compiler':
      default:
        closurePath: "#{__dirname}/.."
        js: 'public/script.js'
        jsOutputFile: 'public/script.min.js'
        options:
          compilation_level: 'ADVANCED_OPTIMIZATIONS'
          language_in: 'ECMASCRIPT5_STRICT'
          warning_level: 'verbose'
          summary_detail_level: 3


  ####
  ## Custom tasks
  ####

  grunt.registerTask 'dev', devTasks.concat ['connect', 'watch']

  grunt.registerTask 'build', compileTasks

  grunt.registerTask 'link', 'dom_munger'

  grunt.registerTask 'optimize', ->
    # TODO: turn on after our refactoring
    #if NODE_ENV isnt 'dev'
    #  grunt.task.run(optimizeScriptTasks)

  grunt.registerTask 'updateComponent', ->
    manifest = grunt.file.readJSON('component.json')
    manifest.scripts = grunt.file.expand('app/**/*.js')
    manifest.styles = grunt.file.expand('app/**/*.styl')
    manifest.templates = grunt.file.expand('app/**/*.jade')

    content = JSON.stringify(manifest, null, '  ')
    grunt.file.write('component.json', content)

  grunt.registerTask 'tag', ->
    grunt.task.run("shell:tag:#{thisManifest.version}")

  grunt.registerTask 'inject', (type) ->
    switch type
      when 'version'
        appName = thisManifest.name
        version = thisManifest.version
        task = "shell:writeVersion:#{appName}:#{version}"
        grunt.task.run(task)

  grunt.registerTask 'compile', (type) ->
    switch type
      when 'js'
        grunt.task.run('copy:js')

      when 'stylus'
        # Compile with or without variable injection
        if grunt.task.exists(styleVariableFile)
          grunt.task.run('stylus:withVariables')
        else
          grunt.task.run('stylus:noVariables')

        if grunt.task.exists('tmp/style.css')
          grunt.task.run('autoprefixer')

        # Optimize only when we're in production and if there are stylesheets
        if NODE_ENV isnt 'dev' and
           grunt.task.exists('public/style.css')
          grunt.task.run('cssshrink')

      when 'jade'
        grunt.task.run(templateTasks)

      # Compile dependencies
      when 'deps'
        runOnDeps('build')
