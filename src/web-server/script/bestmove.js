
/*
Parses a best move response
*/

var BestMove = function(paramStr) {

    function parse(paramStr) {
	var parser = new Parser(paramStr);
	result.column = parser.asInteger(parser.getNth(0));
	return result;
    };

    var fields = parse(paramStr);

    this.isColumn = function() {
	return fields.column != null ? true : false;
    };

    this.getColumn = function() {
	return fields.column;
    };
};



