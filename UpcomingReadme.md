# Connect4
#### A Common Lisp implementation of the Connect Four game. Play against the computer via the console client or your Web-Browser.

### Installation

Clone the project and add the following systems to your asdf-system-lookup path:

```bash
connect4
connect4-console
connect4-cfi
connect4-web-server
```

Used libraries (all loadable via quicklisp):

```bash
cl-ppcre
alexandria
queues
cl-who
hunchentoot
hunchensocket
cl-svg
```

### Run the console client

```bash
sbcl
* (asdf:load-system "connect4-console")
* (connect4-console:lets-play-colorful)
```

## Screenshots

![Screenshot Won](https://raw.github.com/frechmatz/connect4/master/doc/gameplay.jpg)

