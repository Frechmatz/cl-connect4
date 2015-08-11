# Connect4
#### A Common Lisp implementation of the Connect Four game with customizable board size and difficulty level.

### Installation

To use the program you need an implementation of Common Lisp such as clisp (http://www.clisp.org) or sbcl (http://www.sbcl.org).

Download the project via

```bash
git clone https://github.com/Frechmatz/connect4.git
```

or manually download all .lisp files situated in the root of this project.

### Running the program

#### No package managers et.al. installed, just a plain lisp implementation

Enter into the directory into which you've downloaded the sources.
Then start your Lisp implementation and enter the following commands

```bash
(load "board.lisp")
(load "boardformatter.lisp")
(load "engine.lisp")
(load "main.lisp")
# Terminal doesn't support ansi color escape sequences
(lets-play)
# Terminal supports ansi color escape sequences
# (lets-play-colorful)
```
#### Using Quicklisp or ASDF 

Add connect4 to the asdf system path. The system is loaded and started via

```bash
(require "connect4")
# Terminal doesn't support ansi color escape sequences
(lets-play)
# Terminal supports ansi color escape sequences
# (lets-play-colorful)
```
or alternatively via

```bash
(asdf:load-system "connect4")
# Terminal doesn't support ansi color escape sequences
(lets-play)
# Terminal supports ansi color escape sequences
# (lets-play-colorful)
```

### Screenshots

![Screenshot Won](https://raw.github.com/frechmatz/connect4/master/doc/computerwon_2.jpg)

