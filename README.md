# Connect4
#### A Common Lisp implementation of the Connect Four game with customizable board size and difficulty level.

### Installation

Download the project
```bash
git clone https://github.com/Frechmatz/connect4.git
```
Add connect4 to your asdf system path

### Running the program

To run the program you need an implementation of Common Lisp such as clisp (http://www.clisp.org) or sbcl (http://www.sbcl.org).
Enter into the directory where you've downloaded the sources and start the Lisp REPL. 

#### Using quicklisp package manager

On the REPL command prompt enter

```bash
(require "connect4")
(lets-play)
```

#### Using plain asdf (doesn't require a package manager and should work out the box with sbcl or clisp)

On the REPL command prompt enter

```bash
(require "asdf")
(asdf:load-system "connect4")
(lets-play)
```

### Screenshots

![Screenshot Clisp-1](https://raw.github.com/frechmatz/connect4/master/doc/clisp_1_2.jpg)

![Screenshot Clisp-2](https://raw.github.com/frechmatz/connect4/master/doc/clisp_2_2.jpg)

![Screenshot Won](https://raw.github.com/frechmatz/connect4/master/doc/computerwon_2.jpg)

