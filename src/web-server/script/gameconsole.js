
/*
  Console Controller
*/

var GameConsole = function() {


    this.consoleId = 'console-textarea';

    this.write = function(message) {
	var element = document.getElementById(this.consoleId);
	element.value = (element.value.length > 0 ? (element.value + '\n') : '') + message;
	element.scrollTop = element.scrollHeight;
    };


    this.clear = function() {
	var element = document.getElementById(this.consoleId);
	element.value = '';
    };
};



