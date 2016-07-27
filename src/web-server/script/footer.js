
/*
  Controller of the footer.
 */

var Footer = function() {

    var footerId = "footer";
    var clickHereMsg = 'Click here to start a new game.';
    this.activityTimerHandle = null;
    this.activityToggle = null;
    this.initialLevelText = null;
    
    this.setHumanPlayersToken = function(token) {
	var f = document.getElementById(footerId);
	var c = f.querySelector('.human-players-token-indicator');
	c.setAttribute('data-human-players-token', token);
    };

    this.setLevel = function(level) {
	var f = document.getElementById(footerId);
	var c = f.querySelector('.level-indicator p');
	if (!this.initialLevelText) {
	    this.initialLevelText = c.childNodes[0].textContent;
	}
	var text = this.initialLevelText.replace('{level}', level);
	c.childNodes[0].textContent = text;
    };

    this.setClickedLevelHandler = function(handler) {
	var f = document.getElementById(footerId);
	var c = f.querySelector('.level-indicator');
	c.onclick = function(event) {
	    event.preventDefault();
	    handler();
	}.bind(this);
    };
    
    var indicateFinalStateImpl = function(state, message) {
	var f = document.getElementById(footerId);
	var c = f.querySelector('.final-state-click-to-continue');
	c.dataset.value =  state ? state : 'OFF';
	var p = c.querySelector('p');
	p.textContent = message ? message : '';
    };

    this.indicateComputerHasWon = function() {
	indicateFinalStateImpl('COMPUTER_HAS_WON', 'The computer has won. ' + clickHereMsg);
    };
    this.indicateHumanHasWon = function() {
	indicateFinalStateImpl('HUMAN_HAS_WON', 'You have won. ' + clickHereMsg);
    };
    this.indicateDraw = function() {
	indicateFinalStateImpl('DRAW', 'Draw. ' + clickHereMsg);
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
    this.setToggleColorHandler = function(handler) {
	var f = document.getElementById(footerId);
	var c = f.querySelector('.human-players-token-indicator');
	c.onclick = function(event) {
	    event.preventDefault();
	    handler();
	}.bind(this);
    };

    var showQuitButton = function(show) {
	var f = document.getElementById(footerId);
	var c = f.querySelector('.quit-button');
	c.dataset.value =  show ? 'ON' : 'OFF';
    }
    
    this.setQuitHandler = function(handler) {
	var f = document.getElementById(footerId);
	var c = f.querySelector('.quit-button');
	c.onclick = function(event) {
	    event.preventDefault();
	    handler();
	}.bind(this);
    };

    var setActivityStatus = function(value) {
	var f = document.getElementById(footerId);
	var c = f.querySelector('.activity-indicator');
	c.dataset.value =  value;
    };
    
    this.startActivity = function() {
	if(this.activityTimerHandle) {
	    return;
	}
	var that = this;
	this.activityToggle = '1';
	// Wait n seconds and then update once per second
	this.activityTimerHandle = setTimeout(function() {
	    that.activityTimerHandle = setInterval(function() {
		that.activityToggle = that.activityToggle == '1' ? '0' : '1';
		setActivityStatus(that.activityToggle);
		showQuitButton(true);
	    }, 1000)
	}, 3000);
    };
    
    this.stopActivity = function() {
	if (this.activityTimerHandle) {
	    var h = this.activityTimerHandle;
	    this.activityTimerHandle = null;
	    clearTimeout(h);
	}
	setActivityStatus('OFF');
	showQuitButton(false);
    };
    
};
