

var GameController = function() {
    this.cfiClient = new CfiClient();
    this.humanPlayersToken = null;
    this.locked = false;
    this.processingFinalState = false;
};

GameController.prototype.init = function() {
    this.cfiClient.addListener(new ConsoleListener( ['ping', 'pong']));
    this.cfiClient.init();
    this.setHumanPlayersToken(board.getTokenForX());
    footer.hideFinalGameStateIndicator();
    
    footer.setFinalStateContinueHandler( function() {
	this.processingFinalState = false;
	footer.hideFinalGameStateIndicator();
	gameConsole.clear();
	board.clear();
    }.bind(this));

    footer.setToggleColorHandler( function() {
	console.log('Toggle color');
	if( !this.isBlockButton()) {
	    this.toggleHumanPlayersToken();
	}
    }.bind(this));
    
    this.isBlockButton = function() {
    	return this.locked || this.processingFinalState;
    };

    document.getElementById('link-new-game').onclick = function(event) {
	event.preventDefault();
	if (!this.isBlockButton()) {
	    board.clear();
	    footer.hideFinalGameStateIndicator();
	    gameConsole.clear();
	}
    }.bind(this);

    /*
    document.getElementById('link-toggle-color').onclick = function(event) {
	event.preventDefault();
	if( !this.isBlockButton()) {
	    this.toggleHumanPlayersToken();
	}
    }.bind(this);
    */

    /*
    document.getElementById('link-debug').onclick = function(event) {
	event.preventDefault();
	if (!this.isBlockButton()) {
	    //var placement = board.getCcfiPlacement();
	    //this.cfiClient.sendCommand('play ' + placement + ' x' + ' 1' + '' );
	    this.cfiClient.sendCommand('play oxxxoox/o1oxx2/x1xoo2/2ooo2/2xxo2/3xx2 o 6');
	}
    }.bind(this);
    */

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
    if (this.locked || this.processingFinalState) {
	return;
    }
    var c = board.getCellCoordinate(evt.currentTarget);
    var targetRow = board.findRow(c.x);
    if (targetRow == null || board.isFieldSet(c.x,c.y)) {
	return;
    }
    
    var that = this;
    this.locked = true;
    board.clearFieldMarker();
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
		that.cfiClient.sendCommand(
		    ['play', board.getCcfiPlacement(), that.humanPlayersToken, '1', '--column', c.x].join(' '));
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
		    ['play', board.getCcfiPlacement(), board.toggleToken(that.humanPlayersToken), '6'].join(' '));
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
	    if(finalGameStateReached) {
		that.processingFinalState = true;
	    }
	    that.locked = false;
	});
};
