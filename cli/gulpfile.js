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
  STYLUS_VAR_FILE = CWD + '/app/styl/variables.styl',
  head = fs.readFileSync(path.join(DUPLO, './head.html')),
  hasVariableStylus = fs.existsSync(STYLUS_VAR_FILE),
  finished = false,
  watched = false,
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

function connect () {
  require('gulp-connect').server({
    root: 'app',
    port: PORT,
    livereload: true
  });
}

function cleanBefore (prefix) {
  return gulp
    .src(['public/', 'tmp/'], {read: false})
    .pipe(rimraf({force: true}));
}

function cleanAfter () {
  return gulp
    .src('public/params.js', {read: false})
    .pipe(rimraf({force: true}));
}

function dev () {
  cleanBefore().pipe(es.wait(compile));
}

function devWithHTTP () {
  dev();
  connect();
}

function compile (prefix, callback) {
  var prefix = (typeof prefix === 'string') ? prefix : '';
  (prefix !== '') && gutil.log('compiling ', path.basename(prefix));
  es.merge(
    selectComponents(prefix),
    selectAppAssets(prefix),
    compileJS(prefix),
    compileCSS(prefix),
    compileJadeTemplates(prefix),
    compileDev(prefix),
    compileParams(prefix)
  )
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
  var src = '' +
    util.format('module["%s"] = module["%s"] || {}; module["%s"].version = "%s";\n',
      manifest.name, manifest.name, manifest.name, manifest.version) +
    util.format('module.mode = "%s";\n', NODE_ENV);
  return gulp
    .src(prefix+'app/**/*.js')
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
  if (BUILD_MODE === 'dev') {
    return gulp.src(prefix+'dev/**/*');
  } else {
    return gutil.noop();
  }
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
  return gulp
    .src('components/*')
    .pipe(es.through(function write (dir) {
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

gulp.task('dev', dev);
gulp.task('dev:http', devWithHTTP);
gulp.task('dev:connect', connect);

// gulp.task('build');
// gulp.task('build:deps');
// gulp.task('release:patch');
// gulp.task('release:minor');
// gulp.task('release:major');
