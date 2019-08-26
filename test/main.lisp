
;;;; main.lisp

(in-package #:cl-user)
(defpackage #:cl-tex/tests/main
  (:use #:cl #:rove #:tex)
  (:shadowing-import-from #:rove
                          #:*debug-on-error*))
(in-package #:cl-tex/tests/main)

(deftest string-manipulation-test
  (testing "surround string"
    (ok (equal "aaccbb" (tex::surround-string "aa" "bb" "cc")))))
