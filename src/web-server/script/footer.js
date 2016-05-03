

var Footer = function() {

    var footerId = "footer";

    this.setHumanPlayersToken = function(token) {
	var f = document.getElementById(footerId);
	var c = f.querySelector('.human-players-token-indicator');
	c.setAttribute('data-human-players-token', token);
    };

    var indicateFinalStateImpl = function(state, message) {
	var f = document.getElementById(footerId);
	var c = f.querySelector('.final-state-click-to-continue');
	c.dataset.value =  state ? state : 'OFF';
	var p = c.querySelector('p');
	p.textContent = message ? message : '';
    };

    this.indicateComputerHasWon = function() {
	indicateFinalStateImpl('COMPUTER_HAS_WON', 'The computer has won. Click here to start a new game.');
    };
    this.indicateHumanHasWon = function() {
	indicateFinalStateImpl('HUMAN_HAS_WON', 'You have won. Click here to start a new game.');
    };
    this.indicateDraw = function() {
	indicateFinalStateImpl('DRAW', 'Draw. Click here to start a new game.');
    };
    this.hideFinalGameStateIndicator = function() {
	indicateFinalStateImpl(null, '');
    };
    this.setFinalStateContinueHandler = function(handler) {
	var f = document.getElementById(footerId);
	var c = f.querySelector('.final-state-click-to-continue');
	c.onclick = function(event) {
	    event.preventDefault();
	    handler();
	}.bind(this);

    };
};
