
_ = null;
async = null;
board = null;
footer = null;
cfiClient =null;
controller = null;
layoutController = null;
gameConsole = null;

document.addEventListener(
    "DOMContentLoaded",
    function loaded() {
	_ = new Underscore();
	async = new Async();
	board = new Board(),
	footer = new Footer(),
	gameConsole = new GameConsole();
	cfiClient = new CfiClient();
	layoutController = new LayoutController();
	layoutController.doLayout();
	controller = new GameController();
	controller.init();
    },
    false);

