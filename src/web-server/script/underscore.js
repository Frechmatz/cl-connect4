


var Underscore = function() {

    this.each = function(list, iteratee) {
	for( var i = 0; list && i < list.length; i++) {
	    if( i < list.length) {
		iteratee(list[i], i, list);
	    }
	}
    };

    this.find = function(list, predicate) {
	for( var i = 0; list && i < list.length; i++) {
	    if( i < list.length) {
		var item = list[i];
		if(predicate(item)) {
		    return item;
		}
	    }
	}
	return undefined;
    };


    this.map = function(list, iteratee) {
	var map = [];
	this.each(list, function(i) {
	    map.push(iteratee(i));
	});
	return map;
    };

    //findWhere --> see board::setFieldMarker
};
