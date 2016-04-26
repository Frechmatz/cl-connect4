
/*
Parses a best move response
*/

var BestMove = function(paramStr) {

    this.parser = new Parser(paramStr);
    
    this.isColumn = function() {
	return this.parser.asInteger(this.parser.getNth(0)) != null ? true : false;
    };

    this.getColumn = function() {
	return this.parser.asInteger(this.parser.getNth(0));
    };

    this.isFour1 = function() {
	var f = this.parser.get('--four');
	return f && f.split('/').length == 1; 
    };

    this.getLine = function() {
	var l = this.parser.get('--line');
	if (!l) {
	    return [];
	}
	var tuples = l.split('/');
	var arr = [];
	var h = board.getSize().height;
	_.each(tuples, function(tuple) {
	    var c = tuple.split(';');
	    arr.push( {
		x: this.parser.asInteger(c[0]),
		y: h - 1 - this.parser.asInteger(c[1])
	    });
	}.bind(this));
	return arr;
    };
};



