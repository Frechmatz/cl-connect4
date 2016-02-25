

var Footer = function() {

    var footerId = "footer";

    this.setHumanPlayersToken = function(token) {
	var f = document.getElementById(footerId);
	var c = f.querySelector('.human-players-token-indicator');
	c.setAttribute('data-human-players-token', token);
    };

};
