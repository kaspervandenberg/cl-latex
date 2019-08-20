;;;; cl-tex.asd

(asdf:defsystem #:cl-tex
  :description "LaTeX interface for Common Lisp."
  :author "Stefan Devai <stedevai@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:str)
  :components ((:file "src/package")
               (:file "src/cl-tex")))
