;;;; ========================================================================================== ;;;;
;;;; integration.lisp                                                                           ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:cl-latex/tests)

(defvar *latex-object-result* "\\documentclass[12pt]{article}~%\\usepackage{crimson}~%\\usepackage[utf8]{inputenc}~%\\usepackage{float}~%\\usepackage{enumitem}~%\\usepackage{wrapfig}~%~%\\setlist[itemize]{label=textbullet}~%\\setlength{\\parskip}{1em}~%~%\\usepackage{hyperref}~%\\usepackage[T1]{fontenc}~%~%\\begin{document}~%~%\\section*{Menhirs en Bretagne}~%This is some text~%~%~%\\end{document}~%")
(defvar *latex-macro-result* "\\documentclass[12pt]{article}~%\\usepackage{crimson}~%\\usepackage[utf8]{inputenc}~%\\usepackage{float}~%\\usepackage{enumitem}~%\\usepackage{wrapfig}~%~%\\setlist[itemize]{label=textbullet}~%\\setlength{\\parskip}{1em}~%~%\\begin{document}~%~%\\section*{Menhirs en Bretagne}~%This is some text~%~%\\end{document}~%")

(deftest latex-macro-generation
  (testing "LaTeX generation with a macro"
    (let ((latex-string (latex :documentclass (:class :article :options "12pt")
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
                                  "~%"))))
      
      (ok (equal (format nil *latex-macro-result*)
                 latex-string)))))

(deftest latex-object-generation
  (testing "LaTeX generation with a latex-document object"
    (let ((latex-document (make-latex :documentclass '(:class :article :options "12pt")
                           :packages '((:name "crimson")
                                       (:name "inputenc" :options "utf8")
                                       (:name "float")
                                       (:name "enumitem")
                                       (:name "wrapfig"))

                           :preamble (list "\\setlist[itemize]{label=\textbullet}"
                                           (setlength "\\parskip" "1em"))
                           
                           :body (list (section* "Menhirs en Bretagne")
                                       "This is some text"))))
      
      (ok (equal (format nil *latex-object-result*)
                 (to-string latex-document))))))
