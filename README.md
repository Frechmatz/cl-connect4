# Connect4
#### A Common Lisp implementation of the Connect Four game. Comes with a Web and a Console client.

## Installation


```bash
cd ~/quicklisp/local-projects
git clone https://github.com/Frechmatz/cl-connect4.git
```

```
(ql:quickload "cl-connect4")
```

## Run the Console-Client

    (connect4-console:lets-play-colorful)

For monochrome output, not using ANSI escape sequences:

    (connect4-console:lets-play)

### Screenshots

![Screenshot Won](https://raw.github.com/frechmatz/connect4/master/screenshots/lowres-console-1.jpg)

[[Hi-Res-Screenshot](https://raw.github.com/frechmatz/connect4/master/screenshots/highres-console-1.jpg)]

## Run the Web-Client

The Web-Client communicates via a Web-Socket with its server, using the CFI protocol [[Connect Four Interface (CFI) specification](https://raw.github.com/frechmatz/connect4/master/doc/cfi-interface.txt)]

Start the server:

    (connect4-web-server:start)

Enter the following Url into the browser to start a game:
    http://localhost:7999

Stop the server:

    (connect4-web-server:stop)

### Screenshots

![Screenshot Won](https://raw.github.com/frechmatz/connect4/master/screenshots/Connect4-2016-10-03-001-lowres.jpg)

[[Hi-Res-Screenshot](https://raw.github.com/frechmatz/connect4/master/screenshots/Connect4-2016-10-03-001-hires.png)]


## The CFI-Server command line client

Mostly for debugging purposes there is also a command line client for the CFI-Server available.

    (connect4-cfi-console:lets-go)

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





