
document.addEventListener(
    "DOMContentLoaded",
    function loaded() {
	document.connect4 = {
	    cfiClient: new CfiClient(),
	    board: new Board(),
	    footer: new Footer(),
	    controller: new GameController()
	};
	document.connect4.controller.init();
	
    },
    false);

