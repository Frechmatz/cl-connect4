

var GameController = function() {

    var board = function() {
	return document.connect4.board;
    };

    var footer = function() {
	return document.connect4.footer;
    };

    var ccfiClient = function() {
	return document.connect4.ccfiClient;
    };
    
    this.init = function() {
	// The token of the human player
	this.setHumanPlayersToken(board().getTokenForX());
	
	ccfiClient().init();

	document.getElementById("link-new-game").onclick = function(event) {
	    event.preventDefault();
	    board().clear();
	}.bind(this);
	
	document.getElementById("link-debug").onclick = function(event) {
	    event.preventDefault();
	    board().setFieldToX(2,3);
	    console.log(board().getCcfiPlacement());
	    this.toggleHumanPlayersToken();
	    var markers = [];
	    markers.push(board().createCoordinate(2,2));
	    markers.push(board().createCoordinate(3,3));
	    board().setFieldMarker(markers);
	}.bind(this);

	document.getElementById("link-play").onclick = function(event) {
	    event.preventDefault();
	    var placement = board().getCcfiPlacement();
	    ccfiClient().sendCommand('position ' + placement + ' x');
	    ccfiClient().sendCommand('go');
	}.bind(this);
	
    };

    this.toggleHumanPlayersToken = function() {
	this.setHumanPlayersToken(board().toggleToken(this.humanPlayersToken));
    };
    this.setHumanPlayersToken = function(token) {
	this.humanPlayersToken = token;
	// Let :hover CSS rules match
	board().setHumanPlayersToken(this.humanPlayersToken);
	footer().setHumanPlayersToken(this.humanPlayersToken);
    };
}; 
