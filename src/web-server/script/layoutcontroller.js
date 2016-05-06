
/*
  This controller is a failed attempt to fix the 
  unresponsive sizing of the board table :(
*/

var LayoutController = function() {

    var boardId = "board";
    var boardTableId = "board-table";
    var resizeTimerHandle = null;

    var layoutBoard = function() {
	var b = document.getElementById(boardId);
	var dx = b.clientWidth;
	var dy = b.clientHeight;
	var tableDimension = (dx > dy ? dy : dx).toString() + "px";
	//console.log('Computed dimension: ' + tableDimension);
	//var t = document.getElementById(boardTableId);
	//t.style.width = tableDimension;
	//t.style.height = tableDimension;

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
    
};
