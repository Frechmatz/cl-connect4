# Connect4
#### A Common Lisp implementation of the Connect Four game. Play against the computer via the Console or the Web-Browser.

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

![Screenshot Won](https://raw.github.com/frechmatz/connect4/master/screenshots/gameplay.jpg)

## Run the Web-Client

Start the server:
```bash
sbcl
* (asdf:load-system "connect4-web-server")
* (connect4-web-server:start)
```
Enter the following Url into your browser to start a game:
```bash
http://localhost:8002
```
Stop the server: 
```bash
* (connect4-web-server:stop)
```

### Screenshots

![Screenshot Won](https://raw.github.com/frechmatz/connect4/master/screenshots/client1.jpg)
