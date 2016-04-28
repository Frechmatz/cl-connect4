
/*
  Board management module
  - Top/Left of board = 0/0, Second row/Second column = 1/1
  - Field status = x | o | null
*/
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
    this.encodeToCcfiPlacement = function(dx, dy, getTokenFn) {
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

    function setFieldMarkerImpl(cell, set) {
	m = cell.querySelector('.board-cell-marker');
	m.style.display = set ? 'block' : 'none';
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
    this.getFieldToken = function(x,y) {
	var b = document.getElementById(tableId);
	var c = b.querySelector('.board-cell[data-column="' + x + '"][data-row="' + y + '"]');
	var t = c.dataset.token;
	return fieldTokenToCcfiToken(t);
    };

    this.getCcfiPlacement = function() {
	var s = this.getSize();
	return this.encodeToCcfiPlacement(s.width, s.height, this.getFieldToken.bind(this));
    };

    this.forEachCell = function(fn) {
	var b = document.getElementById(tableId);
	var c = b.querySelectorAll('.board-cell');
	for( var i = 0; i < c.length; i++) {
	    fn(c.item(i));
	}
    };

    this.getCellCoordinate = function(cell) {
	var c = {
	    x: parseInt(cell.dataset.column),
	    y: parseInt(cell.dataset.row)
	};
	return c;
    };
    
    this.clear = function() {
	this.forEachCell( function(cell) {
	    cell.setAttribute('data-token', '_');
	    setFieldMarkerImpl(cell, false);
	});
    };

    this.setHumanPlayersToken = function(token) {
	this.forEachCell( function(item) {
	    item.setAttribute('data-human-players-token', token);
	});
    };

    function getFieldCell(x,y) {
	var b = document.getElementById(tableId);
	// TODO: replace by id call
	return b.querySelector('.board-cell[data-column="' + x + '"][data-row="' + y + '"]');
    };
    this.setFieldToken = function(x,y,token) {
	//TODO: Assertion against token
	var c = getFieldCell(x,y);
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
	setFieldToken(x,y,ccfiTokenRepresentationX);
    };
    this.setFieldToO = function(x,y) {
	setFieldToken(x,y,ccfiTokenRepresentationO);
    };
    this.clearField = function(x,y) {
	setFieldToken(x,y,ccfiTokenRepresentationEmpty);
    };
    this.isFieldSet = function(x,y) {
	return this.getFieldToken(x,y) != null;
    };
    // Get row 
    this.findRow = function(x) {
	if( this.isFieldSet(x,0)) {
	    return null;
	}
	var y = -1;
	while (y+1 < this.getSize().height && !this.isFieldSet(x,y+1)) {
	    y = y + 1;
	}
	return y;
    };
    function isCoordinate(x,y,coordinateArray) {
	var found = _.find(coordinateArray, function(c) {
	    return (c.x === x && c.y === y);
	});
	return found ? true : false;
    };

    // coordinateArray = array of { x: ..., y: ... } 
    this.setFieldMarker = function(coordinateArray) {
	coordinateArray = _.map(coordinateArray, function(c) {
	    return {
		x: c.x.toString(),
		y: c.y.toString()
	    };
	});
	// TODO: Implement _.findWhere() in order to get rid of isCoordinate() function
	this.forEachCell( function(cell) {
	    var doMark = isCoordinate(cell.getAttribute('data-column'), cell.getAttribute('data-row'), coordinateArray);
	    setFieldMarkerImpl(cell, doMark);
	})};

    this.isMoveAvailable = function() {
	var width = this.getSize().width;
	for( var x = 0; x < width; x++) {
	    if (!this.isFieldSet(x, 0)) {
		return true;
	    }
	}
	return false;
    };
}

