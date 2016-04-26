

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
    if (y == null) {
	return;
    }

    var that = this;
    if( board.isFieldSet(c.x,c.y)) {
	console.log('No way');
    } else {
	this.locked = true;
	async.waterfall(
	    [
		function(cb) {
		    var handle = that.cfiClient.addListener( new BestMoveListener(function(b) {
			that.cfiClient.removeListener(handle);
			if(!b || b.getColumn() == null) {
			    return cb(null, false);
			}
			board.setFieldToken(c.x,y, that.humanPlayersToken);
			if (b.isFour1()) {
			    board.setFieldMarker(b.getLine());
			    alert('You have won');
			    return cb(null, false);
			} else {
			    return cb(null, true);
			}
		    }));
		    that.cfiClient.sendCommand(
			'play ' +
			    board.getCcfiPlacement() +
			    ' ' + that.humanPlayersToken +
			    ' 1' + ' --column ' + c.x);
		},
		function(doContinue, cb) {
		    if(!doContinue) {
			return cb(null, false);
		    };
		    var handle = that.cfiClient.addListener( new BestMoveListener(function(b) {
			that.cfiClient.removeListener(handle);
			if(!b || b.getColumn() == null) {
			    return cb(null, false);
			}
			board.setFieldToken(
			    b.getColumn(),
			    board.findRow(b.getColumn()),
			    board.toggleToken(that.humanPlayersToken));
			if (b.isFour1()) {
			    board.setFieldMarker(b.getLine());
			    alert('Computer has won');
			    return cb(null, false);
			} else {
			    return cb(null, true);
			}
		    }));
		    that.cfiClient.sendCommand(
			'play ' +
			    board.getCcfiPlacement() +
			    ' ' + board.toggleToken(that.humanPlayersToken) +
			    ' 6');
		}
		
	    ],
	    function(err) {
		console.log('Waterfall done');
		that.locked = false;
	    })
    };
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
	this.cfiClient.sendCommand('play ' + placement + ' x' + ' 1' + ' --column 20' );
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
