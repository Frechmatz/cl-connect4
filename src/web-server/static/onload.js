



function testWebSocket()
{
    //websocket = new WebSocket('ws://echo.websocket.org');
    websocket = new WebSocket('ws://localhost:8003/bongo');
    websocket.onopen = function(evt) { onOpen(evt) };
    websocket.onclose = function(evt) { onClose(evt) };
    websocket.onmessage = function(evt) { onMessage(evt) };
    websocket.onerror = function(evt) { onError(evt) };
}

function onOpen(evt)
{
    writeToScreen("CONNECTED");
    doSend("WebSocket rocks");
}

function onClose(evt)
{
    writeToScreen("DISCONNECTED");
}

function onMessage(evt)
{
    writeToScreen('RECEIVED: ' + evt.data);
    websocket.close();
}

function onError(evt)
{
    writeToScreen('ERROR: ' + evt.data);
}

function doSend(message)
{
    writeToScreen("SENT: " + message);
    websocket.send(message);
}

function writeToScreen(message)
{
    var element = document.getElementById('console-textarea');
    element.value = element.value + '\n' + message;
}

function loaded() {
    console.log('Loaded');
    var element = document.getElementById('console-textarea');
    element.value = 'Loaded';
    testWebSocket();
    
};

document.addEventListener("DOMContentLoaded", loaded, false);

