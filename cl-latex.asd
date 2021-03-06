;;;; ========================================================================================== ;;;;
;;;; cl-latex.asd                                                                               ;;;;
;;;; ========================================================================================== ;;;;

(asdf:defsystem #:cl-latex
  :description "Common Lisp library for LaTeX generation."
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
  :in-order-to ((test-op (test-op :cl-latex/tests))))

(asdf:defsystem #:cl-latex/tests
  :description "Test system for cl-tex"
  :author "Stefan Devai <stedevai@gmail.com>"
  :license "MIT"
  :depends-on (:cl-latex :rove)
  :serial t
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "string")
                 (:file "syntax")
                 (:file "integration"))))
  :perform (asdf:test-op (op c) (uiop:symbol-call :rove '#:run c)))
