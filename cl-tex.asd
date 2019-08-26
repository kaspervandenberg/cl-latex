;;;; cl-tex.asd

(asdf:defsystem #:cl-tex
  :description "Common Lisp library to generate LaTeX syntax."
  :author "Stefan Devai <stedevai@gmail.com>"
  :license "MIT"
  :version "0.2.0"
  :serial t
  :components ((:file "source/package")
               (:file "source/cl-tex"))
  :in-order-to ((test-op (test-op :cl-tex/test))))

(asdf:defsystem #:cl-tex/test
  :description "Test system for cl-tex"
  :author "Stefan Devai <stedevai@gmail.com>"
  :license "MIT"
  :depends-on (:cl-tex :rove)
  :components ((:module "test"
                :components
                ((:file "main"))))
  :perform (asdf:test-op (op c) (uiop:symbol-call :rove '#:run c)))
