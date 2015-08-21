# Connect4
#### A Common Lisp implementation of the Connect Four game with customizable board size and difficulty level.

### Installation

To use the program you need an implementation of Common Lisp such as clisp (http://www.clisp.org) or sbcl (http://www.sbcl.org).

Download the project via

```bash
git clone https://github.com/Frechmatz/connect4.git
```

and add connect4 to the asdf system path.

### Running the program

#### Using Quicklisp


```bash
(asdf:load-system "connect4")
# Terminal doesn't support ansi color escape sequences
(connect4:lets-play)
# otherwise
# (connect4:lets-play-colorful)
```

### Screenshots

![Screenshot Won](https://raw.github.com/frechmatz/connect4/master/doc/gameplay.jpg)

