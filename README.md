# Connect4
#### A Common Lisp implementation of the Connect Four game with customizable board size and difficulty level.

### Installation

To use the program you need an implementation of Common Lisp such as clisp (http://www.clisp.org) or sbcl (http://www.sbcl.org).
The following instructions also assume that the quicklisp package manager has been installed.

Download the project
```bash
git clone https://github.com/Frechmatz/connect4.git
```
and add connect4 to the asdf system path

### Running the program

#### Using SBCL

On the lisp command prompt enter

```bash
(require "connect4")
(lets-play)
```

#### Using CLISP 

On the lisp command prompt enter

```bash
(asdf:load-system "connect4")
(lets-play)
```

### Screenshots

![Screenshot Clisp-1](https://raw.github.com/frechmatz/connect4/master/doc/clisp_1_2.jpg)

![Screenshot Clisp-2](https://raw.github.com/frechmatz/connect4/master/doc/clisp_2_2.jpg)

![Screenshot Won](https://raw.github.com/frechmatz/connect4/master/doc/computerwon_2.jpg)

