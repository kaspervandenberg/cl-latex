
;;;; main.lisp

(in-package #:cl-user)
(defpackage #:cl-latex/tests/main
  (:use #:cl #:rove #:tex)
  (:shadowing-import-from #:rove
                          #:*debug-on-error*))

(in-package #:cl-latex/tests/main)

(defparameter *latex-object-result* "\\documentclass[12pt]{article}~%\\usepackage{crimson}~%\\usepackage[utf8]{inputenc}~%\\usepackage{float}~%\\usepackage{enumitem}~%\\usepackage{wrapfig}~%~%\\setlist[itemize]{label=textbullet}~%\\setlength{\\parskip}{1em}~%~%\\usepackage{hyperref}~%\\usepackage[T1]{fontenc}~%~%\\begin{document}~%~%\\section*{Menhirs en Bretagne}~%This is some text~%~%~%\\end{document}~%")
(defparameter *latex-macro-result* "\\documentclass[12pt]{article}~%\\usepackage{crimson}~%\\usepackage[utf8]{inputenc}~%\\usepackage{float}~%\\usepackage{enumitem}~%\\usepackage{wrapfig}~%~%\\setlist[itemize]{label=textbullet}~%\\setlength{\\parskip}{1em}~%~%\\begin{document}~%~%\\section*{Menhirs en Bretagne}~%This is some text~%~%\\end{document}~%")

(deftest latex-macro-generation
  (testing "LaTeX generation with a macro"
	(ok (equal (format nil *latex-macro-result*)
			   (latex :documentclass (:class :article :options "12pt")
					  :packages ((:name "crimson")
								 (:name "inputenc" :options "utf8")
								 (:name "float")
								 (:name "enumitem")
								 (:name "wrapfig"))
					  
					  :preamble ("\\setlist[itemize]{label=\textbullet}"
								 (setlength "\\parskip" "1em"))
					  
					  :body (begin-end "document" "~%"
									   (section* "Menhirs en Bretagne")
									   "This is some text"
									   "~%"))))))

(deftest latex-object-generation
  (testing "LaTeX generation with a latex-document object"
	(ok (equal (format nil *latex-object-result*)
			   (let ((ltx (make-latex :documentclass '(:class :article :options "12pt")
									  :packages '((:name "crimson")
												  (:name "inputenc" :options "utf8")
												  (:name "float")
												  (:name "enumitem")
												  (:name "wrapfig"))

									  :preamble (list "\\setlist[itemize]{label=\textbullet}"
													  (setlength "\\parskip" "1em"))

									  :body (list (section* "Menhirs en Bretagne")
												  "This is some text"))))
				 (to-string ltx))))))

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
    (ok (equal "aa~%bb~%cc~%" (tex::concat-as-lines "aa" nil nil "bb" "cc")))
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
