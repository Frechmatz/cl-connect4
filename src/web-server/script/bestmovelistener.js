

var BestMoveListener = function(callback) {

    this.cmd = "bestmove";
    this.callback = callback;
    
};

BestMoveListener.prototype.onMessage = function(evt) {
    if (evt.data.indexOf(this.cmd) == 0) {
	this.callback(new BestMove(evt.data.substr(this.cmd.length)));
    }
}

