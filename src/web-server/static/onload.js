

var websocket = null;

function testWebSocket()
{
    websocket = new WebSocket('ws://localhost:8003/ccfi');
    websocket.onopen = function(evt) { onOpen(evt) };
    websocket.onclose = function(evt) { onClose(evt) };
    websocket.onmessage = function(evt) { onMessage(evt) };
    websocket.onerror = function(evt) { onError(evt) };
}

function onOpen(evt)
{
    writeToScreen("Connected");
    doSend("ccfi");
}

function onClose(evt)
{
    writeToScreen("DISCONNECTED");
}

function onMessage(evt)
{
    writeToScreen('< ' + evt.data);
}

function onError(evt)
{
    writeToScreen('ERROR: ' + evt.data);
}

function doSend(message)
{
    writeToScreen("> " + message);
    websocket.send(message);
}

function writeToScreen(message)
{
    var element = document.getElementById('console-textarea');
    element.value = element.value + '\n' + message;
}

function loaded() {
    var element = document.getElementById('console-textarea');
    element.value = 'Connecting with CCFI server...';
    testWebSocket();
};

document.addEventListener("DOMContentLoaded", loaded, false);

