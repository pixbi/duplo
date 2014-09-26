'use strict';

var
  fs = require('fs'),
  path = require('path'),
  util = require('util'),
  gulp = require('gulp'),
  jade = require('gulp-jade'),
  file = require('gulp-file'),
  gutil = require('gulp-util'),
  replace = require('gulp-replace'),
  rename = require('gulp-rename'),
  concat = require('gulp-concat'),
  stylus = require('gulp-stylus'),
  rimraf = require('gulp-rimraf'),
  jshint = require('gulp-jshint'),
  prepend = require('gulp-inject-string').prepend,
  wrap = require('gulp-inject-string').wrap,
  autoprefixer = require('gulp-autoprefixer'),
  async = require('async'),
  es = require('event-stream');

var
  PORT = process.env.PORT || 8888,
  DUPLO = process.cwd(),
  CWD = process.env.CWD || '~',
  NODE_ENV = process.env.NODE_ENV || 'dev',
  BUILD_MODE = process.env.BUILD_MODE || 'dev',
  BUILD_FORM = process.env.BUILD_FORM,
  STYLUS_VAR_FILE = CWD + '/app/styl/variables.styl',
  head = fs.readFileSync(path.join(DUPLO, './head.html')),
  hasVariableStylus = fs.existsSync(STYLUS_VAR_FILE),
  finished = false,
  watched = false,
  onlyBuild = false,
  manifest = getManifest();

var styleFiles = [
  'app/styl/keyframes.styl',
  'app/styl/fonts.styl',
  'app/styl/reset.styl',
  'app/styl/main.styl',
  'app/modules/**/index.styl',
  'app/modules/**/*.styl'
];

process.chdir(CWD);

function cleanBefore (prefix) {
  return gulp
    .src(['public/', 'tmp/', 'components/**/public/'], {read: false})
    .pipe(rimraf({force: true}));
}

function cleanAfter () {
  if (BUILD_MODE === 'dev') return;
  return gulp
    .src([
      'public/params.js*',
      'public/template.html'
    ], {read: false})
    .pipe(rimraf({force: true}));
}

function compile (prefix, callback) {
  var prefix = (typeof prefix === 'string') ? prefix : '';
  (prefix !== '') && gutil.log('compiling ', path.basename(prefix));

  // build list for step1
  var list = [
    selectComponents(prefix),
    selectAppAssets(prefix),
    compileJS(prefix),
    compileCSS(prefix),
    compileJadeTemplates(prefix)
  ];
  if (BUILD_MODE === 'dev') {
    list.push(compileDev(prefix));
  }
  list.push(compileParams(prefix));

  es.merge.apply(es, list)
    .pipe(gulp.dest(prefix+'public'))
    .pipe(es.wait(function onend () {
      (prefix !== '') && gutil.log('ok ', path.basename(prefix));
      (typeof callback === 'function') && callback();
      finish();
    }));
}

function finish () {
  // because of the flat structure in componentjs, just called once here
  // so don't need `prefix`.
  if (finished) return;
  finished = true;
  compileDeps(function () {
    es.merge(
      concatJS(),
      concatCSS(),
      concatHTML()
    )
    .pipe(gulp.dest('./public'))
    .pipe(es.wait(function onfinished () {
      gutil.log('Finished basic build');
      cleanAfter();
      // if only need to build, we should quit here right now
      if (onlyBuild) return;
      watch();
      // set finished to false, when build whole once
      // and ready for next build by watch.on('change', fn)
      finished = false;
    }));
  });
}

function selectComponents (prefix) {
  return gulp
    .src(prefix+'components/**/app/assets/**/*')
    .pipe(rename(function (path) {
      path.dirname = path.dirname.replace(/.*\/app\/assets/, '');
    }));
}

function selectAppAssets (prefix) {
  return gulp.src(prefix+'app/assets/**/*');
}

function compileJS (prefix) {
  var manifest = getManifest(prefix);
  var base = /^[a-zA-Z_]{1}\w*$/.test(manifest.name) 
    ? 'module.' + manifest.name
    : 'module["' + manifest.name + '"]';
  var src = '' +
    // use [] to support app name with symbol '-', because module.x-y is invalid in ES
    util.format('%s = %s || {}; %s.version = "%s";\n',
      base, base, base, manifest.version) +
    util.format('module.mode = "%s";\n', NODE_ENV);
  return gulp
    .src(prefix+'app/**/*.js')
    .pipe(jshint())
    .pipe(jshint.reporter('jshint-stylish'))
    // .pipe(jshint.reporter('fail'))
    .pipe(concat('script.js'))
    .pipe(prepend(src));
}

function compileCSS (prefix) {
  var option = {};
  if (hasVariableStylus) {
    option['import'] = 'variables';
    option['include'] = STYLUS_VAR_FILE;
  }
  return gulp
    .src(styleFiles.map(
      function addprefix (file) {
        return prefix + file;
      }
    ))
    .pipe(stylus(option))
    .pipe(concat('style.css'));
}

function compileJadeTemplates (prefix) {
  return gulp
    .src(prefix+'app/index.jade')
    .pipe(jade({
      pretty: true
    }))
    .pipe(rename('template.html'));
}

function compileDev (prefix) {
  return gulp.src(prefix+'dev/**/*');
}

function compileParams (prefix) {
  var basename = prefix + (BUILD_MODE === 'dev' ? 'dev' : 'app');
  return gulp
    .src(basename + '/params.json')
    .pipe(rename('params.js'))
    .pipe(wrap('module.params = ', ';'));
}

function compileDeps (callback) {
  var prefixs = [];
  var excludes = manifest.exclude && manifest.exclude[BUILD_FORM.replace('-', '/')];
  return gulp
    .src('components/*')
    .pipe(es.through(function write (dir) {
      if (excludes) {
        var i;
        var base = path.basename(dir.path).replace('-', '/');
        for (i=0; i<excludes.length; i++) {
          if (base === excludes[i]) return;
        }
      }
      prefixs.push(dir.path + '/');
    }, function end () {
      async.each(prefixs, compile, callback);
    }));
}

function concatJS () {
  var gstream = gulp
    .src([
      'components/pixbi-bootloader/public/script.js',
      'components/**/public/script.js',
      'public/script.js',
      'public/params.js'
    ])
    .pipe(concat('script.js'));

  if (BUILD_MODE !== 'dev') {
    var uglify = require('gulp-uglify');
    gstream = gstream.pipe(uglify());
    gutil.log('uglifying js files...');
  }
  return gstream;
}

function concatCSS () {
  var gstream = gulp
    .src([
      'public/style.css',
      'components/**/public/style.css'
    ])
    .pipe(concat('style.css'))
    .pipe(autoprefixer({
      browsers: ['last 2 Chrome versions', 'last 2 iOS versions', 'ie 10']
    }));

  if (BUILD_MODE !== 'dev') {
    var cssshrink = require('gulp-cssshrink');
    gstream = gstream.pipe(cssshrink());
    gutil.log('cssshrinking...');
  }
  return gstream;
}

function concatHTML () {
  return gulp
    .src([
      'public/template.html',
      'components/**/public/template.html'
    ])
    .pipe(concat('template.html'))
    .pipe(es.through(
      function ondata (template) {
        this.emit('data', template);
        gulp
          .src('public/index.html')
          .pipe(replace(/<\/head>/, head.toString() + '</head>'))
          .pipe(replace(/<\/body>/, template._contents.toString() + '</body>'))
          .pipe(gulp.dest('./public'));
      }
    ));
}

function watch () {
  gutil.log('Watching app');
  if (watched) return;
  watched = true;
  gulp.watch([
    './app/**/*',
    './dev/**/*',
    './components/pixbi-**/app/**/*'
  ], function onappchange (e) {
    cleanBefore().pipe(es.wait(compile));
  });
}

function getManifest (prefix) {
  var manifest;
  var p = (prefix && prefix !== '')
    ? path.join(prefix, './component.json')
    : path.join(CWD, './component.json');
  try {
    manifest = require(p);
  } catch (err) {
    throw new Error('component.json required in ' + p);
  }
  return manifest;
}

function dev (prefix) {
  cleanBefore().pipe(es.wait(function () {
    compile(prefix);
  }));
}

function connect () {
  require('gulp-connect').server({
    root: 'public',
    port: PORT,
    livereload: true,
    fallback: 'index.html'
  });
}

function devWithHTTP () {
  dev();
  connect();
}

function build () {
  onlyBuild = true;
  dev();
}

function release (level) {
  // gulp-bump would use `patch` as its default value
  var bump = require('gulp-bump');
  return gulp
    .src('./component.json')
    .pipe(bump({type: level}))
    .pipe(es.through(
      function ondata (file) {
        var self = this;
        var componentjs = initComponentJSON(file._contents);
        es.merge(
          updateComponentField(componentjs, './app/**/*.js', 'scripts'),
          updateComponentField(componentjs, './app/**/*.styl', 'styles'),
          updateComponentField(componentjs, ['app/**/*.jade', 'app/**/*.html'], 'templates'),
          updateComponentField(componentjs, 'app/assets/images/**/*.*', 'images'),
          updateComponentField(componentjs, 'app/assets/fonts/**/*.*', 'fonts')
        )
        .pipe(es.wait(function () {
          file._contents = new Buffer(JSON.stringify(componentjs, null, 2));
          file.pipe(fs.createWriteStream(file.path));
        }));
      }
    ));
}

function initComponentJSON (buffer) {
  var json = JSON.parse(buffer.toString());
  json['scripts'] = [];
  json['styles'] = [];
  json['templates'] = [];
  json['images'] = [];
  json['fonts'] = [];
  return json;
}

function updateComponentField (json, glob, name) {
  return gulp
    .src(glob, {read: false})
    .pipe(es.through(function onfile (file) {
      json[name].push(path.relative(CWD, file.path));
    }));
}

// for development
gulp.task('dev', devWithHTTP);
gulp.task('dev:connect', connect);

// for production
gulp.task('build', build);
gulp.task('release:patch', release);
gulp.task('release:minor', release.bind(release, 'minor'));
gulp.task('release:major', release.bind(release, 'major'));
