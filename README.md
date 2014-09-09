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

Installation
------------

`contrib` directory contains simple script for openrc and config file for my machine.

Sample
------

My site [rayslava.com](http://rayslava.com) uses this engine for web.
Also it runs on ODroid U2 with Gentoo. I can't imagine more geek example :)
