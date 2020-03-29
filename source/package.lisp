;;;; ========================================================================================== ;;;;
;;;; package.lisp                                                                               ;;;;
;;;; ========================================================================================== ;;;;

(defpackage #:tex
  (:use #:cl)
  (:export #:latex
           #:make-latex
           #:to-string
           #:element
           #:begin-end
           #:setlength
           #:include
           #:includeonly
           #:input
           #:document
           #:pagestyle
           #:thispagestyle
           #:title
           #:author
           #:date
           #:thanks
           #:section
           #:subsection
           #:subsubsection
           #:section*
           #:subsection*
           #:subsubsection*
           #:enumerate
           #:itemize
           #:item
           #:textbf
           #:emph
           #:textit
           #:underline
           #:noindent
           #:uppercase
           #:graphicspath
           #:includegraphics
           #:figure
           #:caption))
