
document.addEventListener(
    "DOMContentLoaded",
    function loaded() {
	document.connect4 = {};
	
	document.connect4.ccfiServer = new CcfiServer();
	document.connect4.board = new Board();

	document.connect4.ccfiServer.init();
	console.log(document.connect4.board.getCcfiPlacement());

	document.getElementById("link-new-game").onclick = function(event) {
	    event.preventDefault();
	    document.connect4.board.clear();
	};
	
	document.getElementById("link-debug").onclick = function(event) {
	    event.preventDefault();
	    document.connect4.board.setFieldToX(2,3);
	    console.log(document.connect4.board.getCcfiPlacement());
	};
    },
    false);

