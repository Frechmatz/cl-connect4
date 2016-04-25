

var GameController = function() {

    this.cfiClient = new CfiClient();
    this.humanPlayersToken = null;
};

GameController.prototype.cellClickHandler = function(evt) {
    var c = board.getCellCoordinate(evt.currentTarget);
    if( board.isFieldSet(c.x,c.y)) {
	console.log('No way');
    } else {
	var y = board.findRow(c.x);
	if(y != null) {
	    board.setFieldToken(c.x,y, this.humanPlayersToken);
	}
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
