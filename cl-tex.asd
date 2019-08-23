;;;; cl-tex.asd

(asdf:defsystem #:cl-tex
  :description "Common Lisp library to generate LaTeX syntax."
  :author "Stefan Devai <stedevai@gmail.com>"
  :license "MIT"
  :version "0.2.0"
  :serial t
  :components ((:file "source/package")
               (:file "source/cl-tex")))
