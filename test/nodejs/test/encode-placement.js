
var needle = require('needle');
var assert = require('assert');

var url = 'http://localhost:8002/script/board.js';

var board1 = [
    [ '_', '_', '_', '_' ],
    [ '_', '_', 'x', 'o' ],
    [ 'x', 'x', 'o', 'o' ]
];

var tokenToCcfi = function(t) {
    if (t == '_' ) {
	return null;
    }
    return t == 'x' ? "x" : "o";
};

describe('EncodePosition', function() {
    it('Loading script...', function(done) {
	needle.get(url, function(error, response) {
	    if (!error && response.statusCode == 200) {
		eval(response.body);
		var board = new Board();
		var b = board.encodeToCcfiPlacement(4, 3, function(x,y) {
		    var f = board1[y][x];
		    return tokenToCcfi(f);
		});
		console.log('Board: ' + b);
		assert.equal( b, "xxoo/2xo/4");
	    } else {
		assert.equal(1,0,'Script could not be loaded' );
	    }
	    done();
	});
    });
});

