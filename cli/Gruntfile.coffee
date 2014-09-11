_ = require('lodash')

DUPLO = process.cwd()
CWD = process.env.CWD or '~'
# User-configurable mode
NODE_ENV = process.env.NODE_ENV or 'dev'
# Always `dev` in dev mode and `prod` in build mode
BUILD_MODE = process.env.BUILD_MODE or 'dev'
BUILD_FORM = 'default'
# Server port
PORT = process.env.PORT || 8888

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

  # Run a task over the repo's dependencies
  runOnDeps = (task) ->
    paths = grunt.file.expand('./components/*')

    for path in paths
      whenExists path, (path) ->
        grunt.task.run("exec:duplo:#{task}:#{path}:#{BUILD_FORM}")

  # Clean unwanted dependencies
  cleanDeps = ->
    manifest = grunt.file.readJSON('./component.json')
    forms = manifest.exclude or {}
    deps = forms[BUILD_FORM] or {}

    for dep in deps
      # Component.IO convention
      dep = dep.replace(/\//g, '-')

      whenExists "components/#{dep}/public", (path) ->
        grunt.file.delete path

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
    'clean:public'
    'clean:tmp'

    'copyComponentAssets'
    'copy:assets'

    'compile:js'
    'compile:stylus'
    'compile:jade'
    'compile:deps'
    'compile:cleanDeps'
    'inject:version'

    'concat:js'
    'concat:css'
    'concat:html'

    'autoprefixer'

    'inject:mode'
    'inject:dev'
    'inject:params'
    'concat:params'

    'replace:link'
    'optimize'

    'clean:tmp'
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
  optimizeTasks = [
    'optimizeStyle'
    'uglify'
  ]


  grunt.initConfig

    ## General

    watch:
      options:
        interrupt: true
        atBegin: true
        livereload: true
      default:
        files: [
          'app/**/*'
          'components/**/app/**/*'
          'dev/**/*'
        ]
        tasks: compileTasks

    clean:
      public: 'public'
      tmp: 'tmp'
      build: [
        'public/template.html'
        'public/params.json'
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
        src: '**/*'
        dest: 'public/'

      js:
        expand: true
        cwd: 'app/'
        src: '**/*.js'
        dest: 'tmp/'

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
          'public/style.css'
          'components/**/public/style.css'
        ]
        dest: 'public/style.css'

      html:
        src: [
          'public/template.html'
          'components/**/public/template.html'
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
          port: PORT
          hostname: '*'
          base: 'public'

    bump:
      options:
        files: ['component.json']
        updateConfigs: []
        commit: false
        createTag: false
        push: false

    exec:
      writeVersion:
        cmd: (appName, version) ->
          base = "module.#{appName}"
          appName = appName.replace(/-/g, '_')
          content = "#{base} = #{base} || {}; #{base}.version = \"#{version}\";"
          "mkdir -p tmp; echo '#{content}' >> tmp/version.js"

      writeMode:
        cmd: (mode) ->
          "echo 'module.mode = \"#{mode}\";' >> tmp/script.js"

      precommit:
        cmd: ->
          chainCommands [
            'git stash'
            'git checkout develop'
          ]

      commit:
        cmd: ->
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
        cmd: (task, path) ->
          path = "#{process.cwd()}/#{path}"
          "cd #{path}; duplo #{task}"

    replace:
      link:
        files:
          'public/index.html': 'public/index.html'
        options:
          patterns: [
            match: /<\/head>/
            replacement: ->
              grunt.file.read("#{DUPLO}/cli/head.html") + '</head>'
          ,
            match: /<\/body>/
            replacement: ->
              grunt.file.read('public/template.html') + '</body>'
          ]


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
        src: 'public/style.css'
        dest: 'public/style.css'

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
        src: 'public/script.js'
        dest: 'public/script.js'


  ####
  ## Custom tasks
  ####

  grunt.registerTask 'dev', (role, form) ->
    BUILD_FORM = form || 'default'

    grunt.task.run 'connect', 'watch'

  grunt.registerTask 'build', (role, form) ->
    BUILD_FORM = form || 'default'

    tasks = compileTasks.concat()
    if role is 'root'
      tasks.push.call tasks, 'clean:build'
    grunt.task.run tasks

  grunt.registerTask 'release', (level) ->
    grunt.task.run [
      'exec:precommit'
      "bump:#{level}"
      'updateComponent'
      'exec:commit'
    ]

  grunt.registerTask 'optimize', ->
    if NODE_ENV isnt 'dev'
      grunt.task.run(optimizeTasks)

  grunt.registerTask 'updateComponent', ->
    manifest = grunt.file.readJSON('component.json')
    manifest.scripts = grunt.file.expand('app/**/*.js')
    manifest.styles = grunt.file.expand('app/**/*.styl')
    manifest.templates = grunt.file.expand [
      'app/**/*.jade'
      'app/**/*.html'
    ]
    manifest.images = grunt.file.expand('app/assets/images/**/*.*')
    manifest.fonts = grunt.file.expand('app/assets/fonts/**/*.*')

    content = JSON.stringify(manifest, null, '  ')
    grunt.file.write('component.json', content)

  grunt.registerTask 'inject', (type) ->
    switch type
      when 'version'
        manifest = grunt.file.readJSON('./component.json')
        appName = manifest.name
        version = manifest.version
        task = "exec:writeVersion:#{appName}:#{version}"
        grunt.task.run(task)

      when 'mode'
        grunt.task.run("exec:writeMode:#{NODE_ENV}")

      when 'dev'
        if BUILD_MODE is 'dev'
          grunt.task.run('copy:dev')
          whenExists 'dev/params.json', (path) ->
            grunt.file.copy(path, 'tmp/params.json')
        else
          whenExists 'app/params.json', (path) ->
            grunt.file.copy(path, 'tmp/params.json')

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
          ]
        else
          grunt.task.run [
            'stylus:noVariables'
          ]

      when 'jade'
        grunt.task.run(templateTasks)

      # Compile dependencies
      when 'deps'
        runOnDeps('buildDep')

      # Clean unwanted dependencies
      when 'cleanDeps'
        cleanDeps()

  grunt.registerTask 'optimizeStyle', ->
    if grunt.file.exists('public/style.css')
      grunt.task.run [
        'cssshrink'
      ]

  # We need to manually handle components as the `cwd` path is dynamic
  # depending on the assets
  grunt.registerTask 'copyComponentAssets', ->
    assets = grunt.file.expand 'components/*/app/assets/**/*'
    pattern = /^components\/[^\/]+\/app\/assets\//

    for asset in assets
      assetRelPath = asset.replace(pattern, '')

      if grunt.file.isFile(asset)
        grunt.file.copy asset, "public/#{assetRelPath}"
