;;;; cl-tex.asd

(asdf:defsystem #:cl-latex
  :description "Common Lisp library for LaTeX syntax generation."
  :author "Stefan Devai <stedevai@gmail.com>"
  :license "MIT"
  :version "0.4.1"
  :serial t
  :components ((:module "source"
                :components
                ((:file "package")
                 (:file "string")
                 (:file "syntax" :depends-on ("string"))
                 (:file "cl-latex" :depends-on ("string" "syntax")))))
  :in-order-to ((test-op (test-op :cl-latex/test))))

(asdf:defsystem #:cl-latex/test
  :description "Test system for cl-tex"
  :author "Stefan Devai <stedevai@gmail.com>"
  :license "MIT"
  :depends-on (:cl-latex :rove)
  :components ((:module "test"
                :components
                ((:file "main"))))
  :perform (asdf:test-op (op c) (uiop:symbol-call :rove '#:run c)))
