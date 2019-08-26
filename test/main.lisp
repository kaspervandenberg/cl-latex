
;;;; main.lisp

(in-package #:cl-user)
(defpackage #:cl-tex/tests/main
  (:use #:cl #:rove #:tex)
  (:shadowing-import-from #:rove
                          #:*debug-on-error*))
(in-package #:cl-tex/tests/main)

(deftest string-manipulation-test
  (testing "surround string"
    (ok (equal (tex::surround-string "aa" "cc" "bb")
               "aabbcc"))
    (ok (equal (tex::surround-string "aa" "cc" "bb" "dd")
               "aabbddcc"))
    (ok (equal (tex::surround-string "aa" "cc" (if (> 0 1) "ff" "gg") "ss")
               "aaggsscc"))))

(deftest latex-generation
  (testing "element generation"
    (ok (equal (textbf "test") "\\textbf{test}"))
    (ok (equal (emph "test") "\\emph{test}"))
    (ok (equal (noindent "test") "\\noindent{test}"))
    (ok (equal (uppercase "test") "\\uppercase{test}"))
    (ok (equal (section "test") "\\section{test}"))
    (ok (equal (begin-end "arg" "test") "\\begin{arg}~%test~%\\end{arg}"))
    (ok (equal (begin-end "arg" (section (textbf (emph (noindent (uppercase "test"))))))
               "\\begin{arg}~%\\section{\\textbf{\\emph{\\noindent{\\uppercase{test}}}}}~%\\end{arg}"))))
