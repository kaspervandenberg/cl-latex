#!/usr/bin/sbcl --script

(load #P"~/.quicklisp/setup.lisp")
(ql:quickload :rove)
(ql:quickload :cl-tex)

(asdf:test-system :cl-tex)

