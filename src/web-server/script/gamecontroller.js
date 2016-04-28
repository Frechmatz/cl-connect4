

var GameController = function() {

    this.cfiClient = new CfiClient();
    this.humanPlayersToken = null;
    this.locked = false;
};

GameController.prototype.cellClickHandler = function(evt) {
    if (this.locked) {
	return;
    }
    var c = board.getCellCoordinate(evt.currentTarget);
    var y = board.findRow(c.x);
    if (y == null || board.isFieldSet(c.x,y)) {
	return;
    }
    
    var that = this;
    this.locked = true;
    // Lets go
    async.waterfall(
	[
	    // Check if human player has four pieces in a row
	    function(cb) {
		var handle = that.cfiClient.addListener( new BestMoveListener(function(b) {
		    that.cfiClient.removeListener(handle);
		    if(!b || b.getColumn() == null) {
			return cb(null, false);
		    }
		    board.setFieldToken(c.x,y, that.humanPlayersToken);
		    var isFour = b.isFour1();
		    if (isFour) {
			board.setFieldMarker(b.getLine());
			alert('You have won');
		    };
		    return cb(null, isFour);
		}));
		that.cfiClient.sendCommand(
		    ['play', board.getCcfiPlacement(), that.humanPlayersToken, '1', '--column', + c.x].join(' '));
	    },
	    // Check if draw (no move left board)
	    function(finalGameStateReached, cb) {
		if(!finalGameStateReached) {
		    finalGameStateReached = !board.isMoveAvailable();
		    if (finalGameStateReached) {
			alert('Draw!');
		    }
		}
		cb(null, finalGameStateReached);
	    },
	    // Let computer play its move
	    function(finalGameStateReached, cb) {
		if(finalGameStateReached) {
		    return cb(null, true);
		};
		var handle = that.cfiClient.addListener( new BestMoveListener(function(b) {
		    that.cfiClient.removeListener(handle);
		    if(!b || b.getColumn() == null) {
			return cb(null, true);
		    }
		    board.setFieldToken(
			b.getColumn(),
			board.findRow(b.getColumn()),
			board.toggleToken(that.humanPlayersToken));
		    var isFour = b.isFour1();
		    if (isFour) {
			board.setFieldMarker(b.getLine());
			alert('Computer has won');
		    }
		    return cb(null, isFour);
		}));
		that.cfiClient.sendCommand(
		    ['play', board.getCcfiPlacement(), board.toggleToken(that.humanPlayersToken), '6'].join(' '));
	    },
	    // Check if draw (no move left board)
	    function(finalGameStateReached, cb) {
		if(!finalGameStateReached) {
		    finalGameStateReached = !board.isMoveAvailable();
		    if (finalGameStateReached) {
			alert('Draw!');
		    }
		}
		cb(null, finalGameStateReached);
	    }
	    
	],
	function(err, finalGameStateReached) {
	    console.log('Waterfall done');
	    if(finalGameStateReached) {
		// Do something
	    }
	    that.locked = false;
	});
};

GameController.prototype.init = function() {
    this.cfiClient.addListener(new ConsoleListener( ['ping', 'pong']));

    // The token of the human player
    this.setHumanPlayersToken(board.getTokenForX());
    
    this.cfiClient.init();

    document.getElementById('link-new-game').onclick = function(event) {
	event.preventDefault();
	board.clear();
    }.bind(this);
    
    document.getElementById('link-debug').onclick = function(event) {
	event.preventDefault();
	console.log(board.getCcfiPlacement());
	this.toggleHumanPlayersToken();
	board.setFieldMarker([ { x:2, y:2 }, { x:3, y:3 } ]);
    }.bind(this);

    document.getElementById('link-play').onclick = function(event) {
	event.preventDefault();
	var placement = board.getCcfiPlacement();
	this.cfiClient.sendCommand('play ' + placement + ' x' + ' 1' + '' );
    }.bind(this);

    board.forEachCell( function(cell) {
	cell.onclick = this.cellClickHandler.bind(this);
    }.bind(this));
};



GameController.prototype.toggleHumanPlayersToken = function() {
    this.setHumanPlayersToken(board.toggleToken(this.humanPlayersToken));
};

GameController.prototype.setHumanPlayersToken = function(token) {
    this.humanPlayersToken = token;
    // Let :hover CSS rules match
    board.setHumanPlayersToken(this.humanPlayersToken);
    footer.setHumanPlayersToken(this.humanPlayersToken);
};
