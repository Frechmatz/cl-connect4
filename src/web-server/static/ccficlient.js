

var CcfiClient = function() {
    var websocket = null;
    var consoleId = 'console-textarea';
    var url = 'ws://localhost:8003/ccfi';
    
    function connect() {
	websocket = new WebSocket(url);
	websocket.onopen = function(evt) { onOpen(evt) };
	websocket.onclose = function(evt) { onClose(evt) };
	websocket.onmessage = function(evt) { onMessage(evt) };
	websocket.onerror = function(evt) { onError(evt) };
    };

    function writeToScreen(message) {
	var element = document.getElementById(consoleId);
	element.value = (element.value.length > 0 ? (element.value + '\n') : '') + message;
    };

    function onOpen(evt) {
	writeToScreen("Connected");
	doSend("ccfi");
	if (this.timerId != null) {
	    window.clearInterval(this.timerId);
	    this.timerId = null;
	}
	// Connection-Keep-Alive timer
	this.timerId = window.setInterval(
	    function() {
		doSend('Ping');
	    },
	    30*1000
	) 
    };

    function onClose(evt) {
	writeToScreen("DISCONNECTED");
    }

    function onMessage(evt) {
	writeToScreen('< ' + evt.data);
    }

    function onError(evt) {
	writeToScreen('ERROR: ' + evt.data);
    }

    function doSend(message) {
	writeToScreen("> " + message);
	websocket.send(message);
    }

    this.init = function() {
	this.timerId = null;
	connect();
    };
};
