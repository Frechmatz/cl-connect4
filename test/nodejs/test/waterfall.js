
var needle = require('needle');
var assert = require('assert');

var url = 'http://localhost:7999/script/async.js';
var async = null;

before(function(done) {
    console.log('Loading script...');
    needle.get(url, function(error, response) {
	if (!error && response.statusCode == 200) {
	    eval(response.body);
	    async = new Async();
	} else {
	    assert.equal(1,0,'Script could not be loaded' );
	};
	done();
    });
});

describe('Waterfall', function() {
    it('Test Empty tasks', function(done) {
	async.waterfall(
	    [],
	    function(err, result) {
		assert(!err);
		assert(result == null);
		done();
	    });
    });
    it('Test Synchronous', function(done) {
	async.waterfall( [
	    function(cb) {
		cb(null, "One");
	    },
	    function(one, cb) {
		console.log('Got One');
		assert.equal("One", one);
		cb(null, "Two", "Three");
	    }
	], function(err, two, three) {
	    assert(!err);
	    assert.equal("Two", two);
	    assert.equal("Three", three);
	    done();
	});
    });


    it('Test Asynchronous', function(done) {
	async.waterfall( [
	    function(cb) {
		setTimeout( function() {
		    cb(null, "One");
		}, 500);
	    },
	    function(one, cb) {
		console.log('Got One');
		assert.equal("One", one);
		setTimeout( function() {
		    cb(null, "Two", "Three");
		}, 500);
	    }
	], function(err, two, three) {
	    assert(!err);
	    assert.equal("Two", two);
	    assert.equal("Three", three);
	    done();
	});
    });
    
    it('Test Error handler called', function(done) {
	async.waterfall( [
	    function(cb) {
		setTimeout( function() {
		    cb("Nope", "Error");
		}, 500);
	    },
	    function(one, cb) {
		setTimeout( function() {
		    cb(null);
		}, 500);
	    }
	], function(err, message) {
	    assert.equal("Nope", err);
	    assert.equal("Error", message);
	    done();
	});
    });

    it('Test Empty result', function(done) {
	async.waterfall( [
	    function(cb) {
		setTimeout( function() {
		    cb(null);
		}, 500);
	    },
	    function(cb) {
		setTimeout( function() {
		    cb(null, "Two", "Three");
		}, 500);
	    }
	], function(err, two, three) {
	    assert(!err);
	    assert.equal("Two", two);
	    assert.equal("Three", three);
	    done();
	});
    });

});

