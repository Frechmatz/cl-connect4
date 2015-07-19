# Connect4
#### A Common Lisp implementation of the Connect Four game with customizable board size and difficulty level.

### Installation

To use the program you need an implementation of Common Lisp such as clisp (http://www.clisp.org) or sbcl (http://www.sbcl.org).
The following instructions also assume that the quicklisp package manager has been installed.

Download the project
```bash
git clone https://github.com/Frechmatz/connect4.git
```
and add connect4 to the asdf system path. The system is loaded via

```bash
(require "connect4")
```
or alternatively via

```bash
(asdf:load-system "connect4")
```
### Running the program

```bash
(lets-play)
```

If your terminal doesn't support colors start the game as follows

```bash
(lets-play :colors-not-supported t)
```

### Screenshots

![Screenshot Won](https://raw.github.com/frechmatz/connect4/master/doc/computerwon_2.jpg)

