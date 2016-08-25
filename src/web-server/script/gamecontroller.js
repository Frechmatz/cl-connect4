
/*
  Central Game controller
 */

var GameController = function() {
    this.cfiClient = new CfiClient();
    this.humanPlayersToken = null;
    this.locked = false;
    this.processingFinalState = false;
};

GameController.prototype.init = function() {
    this.level = 6;
    this.cfiClient.addListener(new ConsoleListener( ['ping', 'pong']));
    this.cfiClient.init();
    this.setHumanPlayersToken(board.getTokenForX());
    this.processingFinalState = false;
    
    footer.hideFinalGameStateIndicator();
    footer.stopActivity();
    footer.setLevel(this.level);

    this.finalStateContinue = function() {
	this.processingFinalState = false;
	footer.hideFinalGameStateIndicator();
	gameConsole.clear();
	board.clear();
    };
    
    footer.setFinalStateContinueHandler( function() {
	this.finalStateContinue();
    }.bind(this));

    footer.setToggleColorHandler( function() {
	console.log('Toggle color');
	if( !this.isBlockButton()) {
	    this.toggleHumanPlayersToken();
	}
    }.bind(this));

    footer.setClickedLevelHandler( function() {
	console.log('Cycle Level');
	if( !this.isBlockButton()) {
	    this.level = this.level < 12 ? this.level + 1 : 1;
	    footer.setLevel(this.level);
	}
    }.bind(this));
    
    footer.setQuitHandler( function() {
	console.log('Quit');
	this.cfiClient.sendCommand('quit');
    }.bind(this));
    
    this.isBlockButton = function() {
    	return this.locked /* || this.processingFinalState */;
    };

    document.getElementById('link-new-game').onclick = function(event) {
	event.preventDefault();
	if (!this.isBlockButton()) {
	    this.finalStateContinue();
	}
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
    board.setHumanPlayersToken(this.humanPlayersToken);
    footer.setHumanPlayersToken(this.humanPlayersToken);
};

/*
  Player has clicked into a cell.
*/
GameController.prototype.cellClickHandler = function(evt) {
    if (this.locked) {
	return;
    }
    if (this.processingFinalState) {
	return this.finalStateContinue();
    }
    var c = board.getCellCoordinate(evt.currentTarget);
    var targetRow = board.findRow(c.x);
    if (targetRow == null || board.isFieldSet(c.x,c.y)) {
	return;
    }
    
    var that = this;
    this.locked = true;
    board.clearFieldMarker();
    footer.startActivity();
    async.waterfall(
	[
	    // Apply humans move
	    function(cb) {
		var handle = that.cfiClient.addListener( new BestMoveListener(function(b) {
		    that.cfiClient.removeListener(handle);
		    if(!b || b.getColumn() == null) {
			return cb(null, false);
		    }
		    board.setFieldToken(c.x, targetRow, that.humanPlayersToken);
		    var isFour = b.isFour1();
		    if (isFour) {
			board.setFieldMarker(b.getLine());
			footer.indicateHumanHasWon();
		    };
		    return cb(null, isFour);
		}));
		var curPlacement = board.getCcfiPlacement();
		// Put players move into the board now because
		// waiting for the response results in too much latency
		board.setFieldToken(c.x, targetRow, that.humanPlayersToken);
		that.cfiClient.sendCommand(
		    ['play', curPlacement, that.humanPlayersToken, '1', '--column', c.x].join(' '));
	    },
	    // Check if still a move is available
	    function(finalGameStateReached, cb) {
		if(!finalGameStateReached) {
		    finalGameStateReached = !board.isMoveAvailable();
		    if (finalGameStateReached) {
			footer.indicateDraw();
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
			board.toggleToken(that.humanPlayersToken),
			{ marker: board.toggleToken(that.humanPlayersToken)});
		    var isFour = b.isFour1();
		    if (isFour) {
			board.setFieldMarker(b.getLine());
			footer.indicateComputerHasWon();
		    }
		    return cb(null, isFour);
		}));
		that.cfiClient.sendCommand(
		    ['play', board.getCcfiPlacement(), board.toggleToken(that.humanPlayersToken), that.level].join(' '));
	    },
	    // Check if draw
	    function(finalGameStateReached, cb) {
		if(!finalGameStateReached) {
		    finalGameStateReached = !board.isMoveAvailable();
		    if (finalGameStateReached) {
			footer.indicateDraw();
		    }
		}
		cb(null, finalGameStateReached);
	    }
	],
	function(err, finalGameStateReached) {
	    footer.stopActivity();
	    if(finalGameStateReached) {
		that.processingFinalState = true;
	    }
	    that.locked = false;
	});
};
