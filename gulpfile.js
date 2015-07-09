// Copyright 2015-present Greg Hurrell. All rights reserved.
// Licensed under the terms of the MIT license.

var babel = require('gulp-babel');
var eslint = require('gulp-eslint');
var flow = require('gulp-flowtype');
var gulp = require('gulp');
var gutil = require('gulp-util');

var babelOptions = {
  optional: [
    'es7.asyncFunctions',
    'es7.classProperties',
    'es7.decorators',
    'es7.objectRestSpread',
  ],
};
var watching = false;

/**
 * Ring the terminal bell.
 */
function ringBell() {
  process.stderr.write("\x07");
}

/**
 * Wrap a stream in an error-handler (until Gulp 4, needed to prevent "watch"
 * task from dying on error).
 */
function wrap(stream) {
  stream.on('error', function(error) {
    gutil.log(gutil.colors.red(error.message));
    gutil.log(error.stack);
    if (watching) {
      gutil.log(gutil.colors.yellow('[aborting]'));
      stream.end();
    } else {
      gutil.log(gutil.colors.yellow('[exiting]'));
      process.exit(1);
    }
    ringBell();
  });
  return stream;
}

gulp.task('default', ['watch']);

gulp.task('build', ['js']);

gulp.task('flow', ['typecheck']);

gulp.task('js', function() {
  return gulp.src('src/**/*.js')
    .pipe(eslint())
    .pipe(eslint.format())
    .pipe(wrap(babel(babelOptions)))
    .pipe(gulp.dest('dist'));
});

gulp.task('lint', function() {
  return gulp.src('src/**/*.js')
    .pipe(eslint())
    .pipe(eslint.format())
});

gulp.task('typecheck', function() {
  return gulp.src('src/**/*.js')
    .pipe(flow())
});

gulp.task('watch', function() {
  watching = true;
  gulp.watch('src/**/*.js', ['js']);
});
