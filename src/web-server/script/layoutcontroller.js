
/*
  LayoutController
  - Sets the size of the playfield table according to the actual size of its parent
    (don't know how to this via Css)
  - Registers a global resize handler
*/

var LayoutController = function() {

    var boardId = "board";
    var boardTableId = "board-table";
    var resizeTimerHandle = null;

    var layoutBoard = function() {
	var clientArea = document.getElementById(boardId);
	var clientDx = clientArea.clientWidth;
	var clientDy = clientArea.clientHeight;
	var boardArea = document.getElementById(boardTableId);
	boardArea.style.width = clientDx + 'px';
	boardArea.style.height = clientDy + 'px';
	var bs = board.getSize();
	var borderSpace = Math.max(bs.width, bs.height) * 2;
	var cellSizeX = (clientDx - borderSpace) / (0 + bs.width);
	var cellSizeY = (clientDy - borderSpace) / (0 + bs.height);
	var cellSize = Math.min(cellSizeX, cellSizeY);
	var msg =
	     'clientDx: ' + clientDx +
	    ' clientDy: ' + clientDy +
	    ' boardDx: ' + bs.width +
	    ' boardDy: ' + bs.height +
	    ' cellSize: ' + cellSize
	console.log(msg);
	board.forEachCell( function(cell) {
	     cell.style.width = cellSize;
	     cell.style.height = cellSize;
	});
    };
    
    window.onresize = function(event) {
	if (resizeTimerHandle) {
	    window.clearTimeout(resizeTimerHandle);
	    resizeTimerHandle = null;
	}
	resizeTimerHandle = window.setTimeout(function() {
	    resizeTimerHandle = null;
	    layoutBoard();
	}, 500);
    };

    this.doLayout = function() {
	layoutBoard();
    };
};
