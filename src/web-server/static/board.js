
var Board = function() {
    var tableId = "board-table";
    var ccfiTokenRepresentationEmpty = '_';
    var ccfiTokenRepresentationX = 'x';
    var ccfiTokenRepresentationO = 'o';

    // Converts token rendered into table cell to a ccfi token
    function fieldTokenToCcfiToken(token) {
	if (token == ccfiTokenRepresentationEmpty) {
	    return null;
	}
	if (token == ccfiTokenRepresentationX) {
	    return 'x';
	}
	return 'o';
    };
    
    /*
      Get CCFI placement
      Passes SVG coordinates when calling getTokenFn
     */
    function encodeToCcfiPlacement(dx, dy, getTokenFn) {
	var scanRow = function (y, dx, getTokenFn) {
            var row = '';
            var nilCount = 0;
            for (var x = 0; x < dx; x += 1) {
		var field = getTokenFn(x, y);
		if (!field) {
                    nilCount += 1;
		} else {
                    if (nilCount > 0) {
			row += nilCount;
			nilCount = 0;
                    };
                    row += field;
		};
            };
            if (nilCount > 0) {
		row += nilCount;
            };
            return row;
	};
	var result = '';
	for (var y = 0; y < dy; y += 1) {
            var curRow = scanRow(dy - y - 1, dx, getTokenFn);
            if (result.length > 0) {
		result += '/';
            };
            result += curRow;
	};
	return result;
    };

    // returns { width: n, height: m } n,m >= 0
    this.getSize = function() {
	var b = document.getElementById(tableId);
	return {
	    width: parseInt(b.dataset.width, 10),
	    height: parseInt(b.dataset.height, 10)
	};
    }

    // SVG coordinates
    // returns Ccfi representation of field: null, x, o
    this.getField = function(x,y) {
	var b = document.getElementById(tableId);
	var c = b.querySelector('.board-cell[data-column="' + x + '"][data-row="' + y + '"]');
	var t = c.dataset.token;
	return fieldTokenToCcfiToken(t);
    };

    this.getCcfiPlacement = function() {
	var s = this.getSize();
	return encodeToCcfiPlacement(s.width, s.height, this.getField.bind(this));
    };

    this.forEachCell = function(fn) {
	var b = document.getElementById(tableId);
	var c = b.querySelectorAll('.board-cell');
	for( var i = 0; i < c.length; i++) {
	    fn(c.item(i));
	}
    };
    
    this.clear = function() {
	this.forEachCell( function(item) {
	    item.setAttribute('data-token', '_');
	});
    };

    this.setHumanPlayersToken = function(token) {
	this.forEachCell( function(item) {
	    item.setAttribute('data-human-players-token', token);
	});
    };

    function setFieldImpl(x,y,token) {
	var b = document.getElementById(tableId);
	// TODO: replace by id call
	var c = b.querySelector('.board-cell[data-column="' + x + '"][data-row="' + y + '"]');
	c.setAttribute('data-token', token);
    }; 

    this.getTokenForX = function() {
	return ccfiTokenRepresentationX;
    };
    this.getTokenForO = function() {
	return ccfiTokenRepresentationO;
    };
    this.toggleToken = function(token) {
	return token == ccfiTokenRepresentationX ? ccfiTokenRepresentationO : ccfiTokenRepresentationX;
    };
    this.setFieldToX = function(x,y) {
	setFieldImpl(x,y,ccfiTokenRepresentationX);
    };
    this.setFieldToO = function(x,y) {
	setFieldImpl(x,y,ccfiTokenRepresentationO);
    };
    this.clearField = function(x,y) {
	setFieldImpl(x,y,ccfiTokenRepresentationEmpty);
    };
}

