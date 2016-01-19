# Connect4
#### A Common Lisp implementation of the Connect Four game with customizable board size and difficulty level.

### Installation (via quicklisp)

Download the project

```bash
cd ~/src/lisp
git clone https://github.com/Frechmatz/connect4.git
```

add connect4 and connect4-console to the asdf system path.

```bash
cd ~/quicklisp/local-projects
ln -s ~/src/lisp/connect4 connect4
ln -s ~/src/lisp/connect4 connect4-console
```

Run the program (using sbcl)

```bash
sbcl
* (asdf:load-system "connect4-console")
* (connect4-console:lets-play-colorful)
```

### Screenshots

![Screenshot Won](https://raw.github.com/frechmatz/connect4/master/doc/gameplay.jpg)

