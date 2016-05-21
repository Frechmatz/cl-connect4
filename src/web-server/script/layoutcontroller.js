
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
	var b = document.getElementById(boardId);
	var clientDx = b.clientWidth;
	var clientDy = b.clientHeight;
	var t = document.getElementById(boardTableId);
	t.style.width = clientDx + 'px';
	t.style.height = clientDy + 'px';
	var bs = board.getSize();
	var cellSize = null;
	var isClientPortrait = clientDx < clientDy;
	var isBoardPortrait = bs.height > bs.width;
	if (isClientPortrait && isBoardPortrait) {
	    
	} else if (!isClientPortrait && isBoardPortrait) {
	    
	} else if(isClientPortrait && !isBoardPortrait) {
	    
	} else if(!isClientPortrait && !isBoardPortrait) {
	    
	} else {
	    //throw Error('Something fishery is going on here');
	} 
	// var cellSize = Math.min(clientDx, clientDy) / Math.max(bs.width, bs.height) + "px";
	// console.log(
	//     'clientDx: ' + clientDx +
	// 	' clientDy: ' + clientDy +
	// 	' boardDx: ' + bs.width +
	// 	' boardDy: ' + bs.height +
	// 	' cellSize: ' + cellSize);
	// board.forEachCell( function(cell) {
	//     cell.style.width = cellSize;
	//     cell.style.height = cellSize;
	// });
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
