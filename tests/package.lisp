;;;; ========================================================================================== ;;;;
;;;; package.lisp                                                                               ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:cl-user)
(defpackage #:cl-latex/tests
  (:use #:cl #:rove #:tex)
  (:shadowing-import-from #:rove
                          #:*debug-on-error*))
