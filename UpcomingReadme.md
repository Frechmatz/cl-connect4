# Connect4
#### A Common Lisp implementation of the Connect Four game. Comes with a Web and a Console User-Interface.

## Installation

Clone the project and add the following systems to your asdf-system path:

```bash
connect4
connect4-console
connect4-cfi-server
connect4-web-server
```

Dependencies (all installable via quicklisp):

```bash
cl-ppcre
alexandria
queues
cl-who
hunchentoot
hunchensocket
cl-svg
bordeaux-threads
```
There are no dependencies on third-party JavaScript/CSS libraries

## Run the Console-Client

```bash
sbcl
* (asdf:load-system "connect4-console")
* (connect4-console:lets-play-colorful)
```
For monochrome output, not using ANSI escape sequences:
```bash
* (connect4-console:lets-play)
```

### Screenshots

![Screenshot Won](https://raw.github.com/frechmatz/connect4/master/screenshots/lowres-console-1.jpg)

[[Hi-Res](https://raw.github.com/frechmatz/connect4/master/screenshots/highres-console-1.jpg)]

## Run the Web-Client

The Web-Client communicates via a Web-Socket with its server, using the [[Connect Four Interface (CFI) specification](https://raw.github.com/frechmatz/connect4/master/doc/cfi-interface.txt)]

Start the server:
```bash
sbcl
* (asdf:load-system "connect4-web-server")
* (connect4-web-server:start)
```
Enter the following Url into your browser to start a game:
```bash
http://localhost:7999
```
Stop the server: 
```bash
* (connect4-web-server:stop)
```

### Screenshots

![Screenshot Won](https://raw.github.com/frechmatz/connect4/master/screenshots/Connect4-2016-10-03-001-lowres.jpg)

[[Hi-Res](https://raw.github.com/frechmatz/connect4/master/screenshots/Connect4-2016-10-03-001-hires.png)]


## The CFI-Server command line client

Mostly for debugging purposes there is also a command line client for the CFI-Server available.
It can be used as follows:

```bash
sbcl
* (asdf:load-system "connect4-cfi-console")
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





