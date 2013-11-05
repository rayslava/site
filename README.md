LISP site project
=================

This is just an experiment in building site in pure Common LISP.
At the moment it has a lisp-server, lisp html, lisp css and lisp javascript…

Usage
-----

You need a common lisp interpeter (I tried ECL and SBCL) and quicklisp.
All the needed packages should be downloaded by quicklisp.

```
$ ecl -load piserv.asd
> (ql:quickload "piserv")
> (piserv:start-server 80)
```
