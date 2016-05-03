

var Async = function() {
};

/*
  A probably bug-ridden implementation of the async.js waterfall function
  https://github.com/caolan/async#waterfalltasks-callback
  Runs the tasks array of functions in series, each passing their results to the next in the array. 
  However, if any of the tasks pass an error to their own callback, the next function is not executed, 
  and the main callback is immediately called with the error.
*/
Async.prototype.waterfall = function(tasks, cb) {
    if (!tasks || tasks.length == 0) {
	return cb();
    }
    
    var i = 0;
    var done = false;
    var run = function(argArray) {
	if (done) {
	    return;
	}
	var task = tasks[i++];
	task.apply(
	    task,
	    argArray.concat([ function(err, result) {
		var rest = [].slice.call(arguments,1);
		if (err || i >= tasks.length) {
		    cb.apply(cb, [err].concat(rest));
		    done = true;
		} else {
		    run.apply(null, [ rest ] );
		}
	    }]));
    };
    run.apply(null, [ [] ]);
};
