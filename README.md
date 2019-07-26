# Connect4
#### A Common Lisp implementation of the Connect Four game. Comes with a Web and a Console client.

## Installation

Clone the project and add the following systems to the asdf-system path (all pointing to the root of connect4):

```bash
cl-connect4
cl-connect4-console
cl-connect4-cfi-server
cl-connect4-web-server
cl-connect4-cfi-console
```

Dependencies (all installable via quicklisp):

```bash
cl-ppcre
alexandria
queues
cl-who
hunchentoot
cl-websocket
cl-svg
bordeaux-threads
```

## Run the Console-Client

```bash
sbcl
* (asdf:load-system "cl-connect4-console")
* (connect4-console:lets-play-colorful)
```
For monochrome output, not using ANSI escape sequences:
```bash
* (connect4-console:lets-play)
```

### Screenshots

![Screenshot Won](https://raw.github.com/frechmatz/connect4/master/screenshots/lowres-console-1.jpg)

[[Hi-Res-Screenshot](https://raw.github.com/frechmatz/connect4/master/screenshots/highres-console-1.jpg)]

## Run the Web-Client

The Web-Client communicates via a Web-Socket with its server, using the CFI protocol [[Connect Four Interface (CFI) specification](https://raw.github.com/frechmatz/connect4/master/doc/cfi-interface.txt)]

Start the server:
```bash
sbcl
* (asdf:load-system "cl-connect4-web-server")
* (connect4-web-server:start)
Hi there. The web server has been started.
The server can be reached via http://localhost:7999
The websocket server has been started.
The websocket server can be reached via http://localhost:7998
```
Enter the following Url into the browser to start a game:
```bash
http://localhost:7999
```
Stop the server: 
```bash
* (connect4-web-server:stop)
```

### Screenshots

![Screenshot Won](https://raw.github.com/frechmatz/connect4/master/screenshots/Connect4-2016-10-03-001-lowres.jpg)

[[Hi-Res-Screenshot](https://raw.github.com/frechmatz/connect4/master/screenshots/Connect4-2016-10-03-001-hires.png)]


## The CFI-Server command line client

Mostly for debugging purposes there is also a command line client for the CFI-Server available.

```
sbcl
* (asdf:load-system "cl-connect4-cfi-console")
* (connect4-cfi-console:lets-go)
Instantiated server
Enter Cfi command
start
started
Enter Cfi command
ping
pong
Enter Cfi command
play 4/4/4/4 x 6
Enter Cfi command
bestmove 3 --score -0.0000012109375
stop
Stopping server...
Stopping server...
Server stopped
NIL
*
```





