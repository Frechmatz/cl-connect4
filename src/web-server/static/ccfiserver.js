

var CcfiServer = function() {
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
	element.value = element.value + '\n' + message;
    };

    function onOpen(evt) {
	writeToScreen("Connected");
	doSend("ccfi");
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
	connect();
    };
};
