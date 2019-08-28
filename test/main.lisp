
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
    (ok (equal (tex::surround-string "ss" "aa" nil)
               "ssaa"))
    (ok (equal (tex::surround-string nil "aa" "ss")
               "ssaa"))
    (ok (equal (tex::surround-string "aa" nil "ss")
               "aass")))

  (testing "concat only strings"
    (ok (equal "aabb" (tex::concat "aa" "bb")))
    (ok (equal "aabb11" (tex::concat "aa" "bb" "11")))
    (ok (equal "aabb112233445566" (tex::concat "aa" "bb" "11" "22" "33" nil "44" "55" "66")))
    (ok (signals (tex::concat "aa" "bb" 'ab) 'type-error))
    (ok (signals (tex::concat "aa" 11) 'type-error)))

  (testing "concat anything as string"
    (ok (equal "aa23.3" (tex::concat-as-string nil "aa" nil 2 3.3 nil)))
    (ok (equal "" (tex::concat-as-string nil)))
    (ok (equal "2.23.3" (tex::concat-as-string 2.2 3.3)))
    (ok (equal "2.2aa" (tex::concat-as-string 2.2 "aa")))
    (ok (equal "22110" (tex::concat-as-string 22 11 00)))
    (ok (equal "aa11" (tex::concat-as-string "aa" 11))))

  (testing "concat as lines"
    (ok (equal "aabbcc" (tex::concat-as-lines "aa" nil nil "bb" "cc")))
    (ok (equal "" (tex::concat-as-lines nil)))
    (ok (equal "aa~%" (tex::concat-as-lines "aa")))
    (ok (equal "aa~%bb~%cc~%dd~%ee~%ff~%" (tex::concat-as-lines "aa" "bb" "cc" "dd" "ee" "ff")))))

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

(deftest misc
  (testing "formatting options"
    (ok (equal "[width=2,height=4]" (tex::format-options '(width 2 height 4))))))
