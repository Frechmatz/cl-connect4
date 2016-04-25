

var CfiClient = function() {
    var websocket = null;
    var url = 'ws://localhost:8003/ccfi';
    var listeners = [];

    function getListener(id) {
	return _.find(listeners, function(i) {
	    return i.id == id;
	});
    };
    
    function createId() {
	while(true) {
	    var id = (Math.floor(Math.random() * (10000))).toString();
	    if (!getListener(id)) {
		return id;
	    }
	}
    }
    
    function callListeners(fnName, evt) {
	_.each(listeners, function(listener) {
	    var fn = listener.listener[fnName];
	    if (fn) {
		fn.apply(listener.listener, [evt]);
	    }
	}.bind(this));
    };

    this.addListener = function(listener) {
	var l = {
	    id: createId(),
	    listener: listener
	};
	listeners.push(l);
	return listener.id;
    };
    
    function connect() {
	websocket = new WebSocket(url);
	websocket.onopen = function(evt) { onOpen(evt) };
	websocket.onclose = function(evt) { onClose(evt) };
	websocket.onmessage = function(evt) { onMessage(evt) };
	websocket.onerror = function(evt) { onError(evt) };
    };

    function onOpen(evt) {
	callListeners('onOpen', evt);
	if (this.timerId != null) {
	    window.clearInterval(this.timerId);
	    this.timerId = null;
	}
	// Connection-Keep-Alive timer
	this.timerId = window.setInterval(
	    function() {
		doSend('ping', true);
	    },
	    30*1000
	) 
    };

    function onClose(evt) {
	callListeners('onClose', evt);
    }

    function onMessage(evt) {
	callListeners('onMessage', evt);
    }

    function onError(evt) {
	callListeners('onError', evt);
    }

    function doSend(message) {
	callListeners('doSend', message);
	websocket.send(message);
    }

    this.init = function() {
	// this.timerId = null;
	connect();
    };

    this.sendCommand = function(cmd) {
	doSend(cmd);
    };

    this.close = function() {
	var tmp = this.websocket;
	this.websocket = null;
	if( tmp) {
	    tmp.close();
	}
    };
    
};
