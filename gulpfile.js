var gulp = require('gulp')
  , clean = require('gulp-clean')
  , gutil = require('gulp-util')
  , plumber = require('gulp-plumber')
  , purescript = require('gulp-purescript')
  , config = {
      clean: ['dist', 'js', 'externs'],
      purescript: {
        src: [
          'bower_components/purescript-*/src/**/*.purs*',
          'src/**/*.purs.hs'
        ],
        examples: 'examples/**/*.purs.hs',
        dest: 'dist/',
        options: {
          main: 'Example.Main',
          modules: ['Example.Interpreter', 'Example.Main', 'Example.User']
        }
      }
    }
;

function error(e) {
  gutil.log(gutil.colors.magenta('>>>> Error <<<<') + '\n' + e.toString().trim());
  this.emit('end');
}

gulp.task('clean', function(){
  return (
    gulp.src(config.clean, {read: false}).
      pipe(clean())
  );
});

gulp.task('examples', ['clean'], function(){
  return (
    gulp.src([config.purescript.examples].concat(config.purescript.src)).
    pipe(plumber()).
    pipe(purescript.psc(config.purescript.options)).
    on('error', error).
    pipe(gulp.dest(config.purescript.dest))
  );
});

gulp.task('make', function(){
  return (
    gulp.src(config.purescript.src).
    pipe(plumber()).
    pipe(purescript.pscMake()).
    on('error', error)
  );
});


gulp.task('watch', function(cb){
  //gulp.watch([config.purescript.examples].concat(config.purescript.src), ['examples']);
  gulp.watch(config.purescript.src, ['make']);
});

gulp.task('default', ['clean', 'make'], function(){});
