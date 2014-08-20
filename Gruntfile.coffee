module.exports = (grunt) ->
  # Always call explicitly with parameter to allow easy extension in the future
  compileTasks = [
    'clean:public'

    'copy:index'
    'copy:assets'
    'copy:dev'

    'compile:js'
    'compile:stylus'
    'compile:jade'
    'inject:version'

    'concat:js'
    'concat:stylus'
    'concat:jade'
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
    'closureCompiler'
    'uglify'
  ]

  # Load Grunt Plugins.
  require('load-grunt-tasks')(grunt)


  grunt.initConfig
    pkg: grunt.file.readJSON('package.json')

    ## General

    watch:
      default:
        files: ['app/**/*', 'components/**/*', 'dev/**/*']
        tasks: 'build'
        options:
          livereload: true

    clean:
      public:
        src: 'public'
      staging:
        src: 'staging'

    copy:
      index:
        expand: true
        src: 'node_modules/grunt-build/index.html'
        dest: 'public/'
        filter: (filepath) ->
          grunt.file.exists(filepath)

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
          'staging/**/*.css'
          'components/**/staging/*.css'
        ]
        dest: 'public/index.css'
      html:
        src: [
          'staging/**/*.html'
          'components/**/staging/*.html'
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
          'public/embed.css': [
            'app/styl/keyframes.styl'
            'app/styl/fonts.styl'
            'app/styl/normalize.styl'
            'app/styl/main.styl'
            'app/views/**/index.styl'
            'app/modules/**/index.styl'
            'app/components/**/*.styl'
            'app/styl/mobile.styl'
            'app/views/**/mobile.styl'
            'app/modules/**/mobile.styl'
          ]

    autoprefixer:
      default:
        src: 'public/embed.css'
        dest: 'public/embed.css'
        options: 
          browsers: ['last 2 Chrome versions', 'last 2 iOS versions', 'ie 10']

    cssshrink:
      default:
        files:
          'public/embed.css': ['public/embed.css']

    ## Template-specific

    jade:
      options:
        basedir: 'app/'
      default:
        files:
          'build/embed.html': 'app/app.jade'
          'public/index.html': 'dev/dev.jade'

    ## Script-specific

    uglify:
      default:
        files:
          'public/embed.js': 'public/embed.js'

    closureCompiler:
      options:
        compilerFile: 'compiler.jar'
        compilerOpts:
          compilation_level: 'ADVANCED_OPTIMIZATIONS'
          # externs: ['path/to/file.js', '/source/**/*.js']
          # define: ["'goog.DEBUG=false'"]
          warning_level: 'verbose'
          # jscomp_off: ['checkTypes', 'fileoverviewTags']
          # jscomp_warning: 'reportUnknownTypes'
          summary_detail_level: 3
        # d32: true
      default:
        src: 'public/embed.js'
        dest: 'public/embed.min.js'


  ## Custom tasks

  grunt.registerTask 'compile', (type) ->
    # First compile this repo
    switch type
      when 'js'
        grunt.task.run('scriptTasks')
      when 'stylus'
        grunt.task.run('styleTasks')
      when 'jade'
        grunt.task.run('templateTasks')

    # Then compile its dependencies
    runOnDeps('build')

  grunt.registerTask 'check', ['closureCompiler']

  grunt.registerTask 'dev', ['connect', 'watch']

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


  ## Auxiliary constants and functions

  # All the manifest files of this repo and its dependencies
  manifests = do ->
    manifest = grunt.file.readJSON('./component.json')
    deps = Object.keys(manifest.dependencies or {})

    # Find this repo's dependency manifests
    deps.map (dep) ->
      dep = dep.replace('/', '-')
      _manifestPath = "./components/#{dep}/component.json"
      _manifest = grunt.file.readJSON(_manifestPath)
      _manifest.path = "./components/#{dep}/"

  # Run a task over the repo's dependencies
  runOnDeps = (task) ->
    for manifest in manifests
      path = manifest.path or '.'
      grunt.task.run("shell:make:#{task}:#{path}")
