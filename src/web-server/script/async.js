

var Async = function() {

    

};

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
