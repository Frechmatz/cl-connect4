
document.addEventListener(
    "DOMContentLoaded",
    function loaded() {
	document.connect4 = {
	    ccfiClient: new CcfiClient(),
	    board: new Board(),
	    controller: new GameController()
	};
	document.connect4.controller.init();
	
    },
    false);

