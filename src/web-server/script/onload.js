
_ = null;
async = null;
board = null;
footer = null;
cfiClient =null;
controller = null;

document.addEventListener(
    "DOMContentLoaded",
    function loaded() {
	_ = new Underscore();
	async = new Async();
	board = new Board(),
	footer = new Footer(),
	cfiClient = new CfiClient();
	controller = new GameController();
	controller.init();
    },
    false);

