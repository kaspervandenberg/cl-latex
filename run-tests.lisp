#!/usr/bin/sbcl --script

(load #P"~/.quicklisp/setup.lisp")
(ql:quickload :rove)
(ql:quickload :cl-latex)

(asdf:test-system :cl-latex)

