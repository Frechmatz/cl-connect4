# Connect4
#### A Common Lisp implementation of the Connect Four game. Play against the computer via the Console-Client or your Web-Browser.

## Installation

Clone the project and add the following systems to your asdf-system path:

```bash
connect4
connect4-console
connect4-cfi
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
There are no dependencies on third-party JavaScript and CSS libraries, such as jquery or bootstrap.

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

![Screenshot Won](https://raw.github.com/frechmatz/connect4/master/doc/gameplay.jpg)

## Run the Web-Client

First, you need to start the server:
```bash
sbcl
* (asdf:load-system "connect4-web-server")
* (connect4-web-server:start)
```
You can now enter the following Url into your browser to start a game:
```bash
http://localhost:8002
```
To stop the server enter 
```bash
* (connect4-web-server:stop)
```
into the Repl.

### Screenshots

![Screenshot Won](https://raw.github.com/frechmatz/connect4/master/doc/client1.jpg)
