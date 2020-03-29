;;;; ========================================================================================== ;;;;
;;;; syntax.lisp                                                                                ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:cl-latex/tests)

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

(deftest latex-misc
  (testing "formatting options"
    (ok (equal "[width=2,height=4]" (tex::format-options '(width 2 height 4))))))
