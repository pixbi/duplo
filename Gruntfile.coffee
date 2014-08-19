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

    'link:js'
    'link:css'
    'link:html'

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


    # General

    watch:
      default:
        files: ['app/**/*', 'components/**/*', 'dev/**/*']
        tasks: compileTasks
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
        dest: 'public/index.html'

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


    # Style-specific

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

    # Template-specific

    jade:
      options:
        basedir: 'app/'
      default:
        files:
          'build/embed.html': 'app/app.jade'
          'public/index.html': 'dev/dev.jade'

    # Script-specific

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


  # Custom tasks

  grunt.registerTask 'default', ->

  grunt.registerTask 'compile', (type) ->
    'app/**/*.js'
    '!app/**/*.spec.js'

  grunt.registerTask 'link', (type) ->

  grunt.registerTask 'check', ->

  grunt.registerTask 'dev', ->

  grunt.registerTask 'tag', ->
    grunt.task.run("shell:tag:#{manifest.version}")

  grunt.registerTask 'inject', (type) ->
    switch type
      when 'version'
        appName = manifest.name
        version = manifest.version
        task = "shell:writeVersion:#{appName}:#{version}"
        grunt.task.run(task)


  # Auxiliary constants and functions

  # All the manifest files of this repo and its dependencies
  manifests = do ->
    manifest = grunt.file.readJSON('./component.json')
    deps = Object.keys(manifest.dependencies or {})

    # Put self first and add its dependencies' manifests afterward
    [manifest].concat deps.map (dep) ->
      dep = dep.replace('/', '-')
      _manifest = grunt.file.readJSON("./components/#{dep}/component.json")
      _manifest.path = "./components/#{dep}/"

  # Run a task over this repo as well as its dependencies
  runOnAll = (task) ->
    for manifest in manifests
      path = manifest.path or '.'
      grunt.task.run("shell:make:#{task}:#{path}")
