CL-BOILERPIPE is a Common Lisp library for extracting the main content
from web pages like newspaper articles and blog posts. It is used by
[TBRSS][] for expanding truncated articles in feeds.

CL-BOILERPIPE is based on the Java [Boilerpipe][] library, based in
turn on Kohlschütter et al.,
[“Boilerplate Detection using Shallow Text Features”][paper].

Only the simplest version of the Boilerpipe algorithm is implemented
here; I find that it works well enough.

# Setup

You will need [FXML][].

    cd quicklisp/local-projects
    git clone https://github.com/TBRSS/FXML.git

    (ql:register-local-projects)
    (ql:quickload :cl-boilerpipe)

# Usage

Given an HTML string, call:

     (cl-boilerpipe:strip-boilerpipe html)

This returns the main content as another HTML string.

[TBRSS]: https://tbrss.com
[Boilerpipe]: https://github.com/kohlschutter/boilerpipe
[paper]: http://www.l3s.de/~kohlschuetter/boilerplate/
