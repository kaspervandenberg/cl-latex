;;;; ========================================================================================== ;;;;
;;;; syntax.lisp                                                                                ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:tex)

(defvar *document-classes*
  '(:article :ieeetran :proc :report :book :slides :memoir :letter :beamer)
  "Generic document classes that come builtin with LaTeX.")

(defvar linebreak "\\linebreak"
  "Default linebreak sintax in Latex")

(defvar maketitle "\\maketitle")
(defvar tableofcontents "\\tableofcontents")
(defvar newline "\\newline")
(defvar newpage "\\newpage")
(defvar ldots "\\ldots")
(defvar centering "\\centering")
(defvar columnsep "\\columnsep")
(defvar columnwidth "\\columnwidth")
(defvar linewidth "\\linewidth")
(defvar paperwidth "\\paperwidth")
(defvar paperheight "\\paperheight")
(defvar textwidth "\\textwidth")
(defvar textheight "\\textheight")
(defvar unitleght "\\unitleght")

(defun element (args &rest rest)
  (let ((arg1 (if (listp args) (car args) args))
        (arg2 (when (listp args) (cadr args))))
    (surround-string
     (concat "\\" arg1 arg2 "{")
     "}"
     (reduce #'concat-as-string rest))))

(defun % (&rest elements)
  (apply #'concat-as-string "% " elements))

(defun format-options (options)
  "Receive a list of key and values, and return a string with formated options
  for LaTeX.

  Example:
  (format-options '(width 2 height 4))
  ;; => \"[width=2,height=4]\""
  (if (listp options)
      (let ((options-list
              (loop :for v := 1 :then (+ v 2)
                    :for k := 0 :then (+ k 2)
                    :while (< v (length options))
                    :when (elt options v)
                      :collect (concat-as-string (string-downcase (elt options k))
                                                 "="
                                                 (elt options v)))))

        (when options-list (concat "["
                                   (format nil "~{~A~^,~}" options-list)
                                   "]")))
      (concat "[" options "]")))

(defun begin-end (args &rest elements)
  (let ((arg (if (listp args) (car args) args))
        (pre-value (when (listp args) (cadr args)))
        (pos-value (when (listp args) (caddr args))))
    (assert arg)
    (concat (if pre-value
                (element (list "begin" pre-value) arg)
                (element "begin" arg))
            pos-value
            "~%"
            (apply #'concat-as-lines elements)
            (element "end" arg))))

(defun setlength (item amount)
  (element (list "setlength" (concat "{" item "}")) amount))

(defun include (filename)
  (element "include" filename))

(defun includeonly (&rest files)
  (element "includeonly" (format nil "~{~A~^,~}" files)))

(defun input (filename)
  (element "input" filename))

(defun pagestyle (style)
  (element "pagestyle" style))

(defun thispagestyle (style)
  (element "thispagestyle" style))

(defun title (&rest elements)
  (apply #'element "title" elements))

(defun author (&rest elements)
  (apply #'element "author" elements))

(defun date (&rest elements)
  (apply #'element "date" elements))

(defun thanks (&rest elements)
  (apply #'element "thanks" elements))

(defun section (&rest elements)
  (apply #'element "section" elements))

(defun subsection (&rest elements)
  (apply #'element "subsection" elements))

(defun subsubsection (&rest elements)
  (apply #'element "subsubsection" elements))

(defun section* (&rest elements)
  (apply #'element "section*" elements))

(defun subsection* (&rest elements)
  (apply #'element "subsection*" elements))

(defun subsubsection* (&rest elements)
  (apply #'element "subsubsection*" elements))

(defun enumerate (&rest list-items)
  (apply #'begin-end "enumerate" (mapcar #'item list-items)))

(defun itemize (&rest list-items)
  (apply #'begin-end "itemize" (mapcar #'item list-items)))

(defun item (&rest elements)
  (apply #'concat-as-string "\\item " elements))

(defun textbf (&rest elements)
  (apply #'element "textbf" elements))

(defun emph (&rest elements)
  (apply #'element "emph" elements))

(defun textit (&rest elements)
  (apply #'element "textit" elements))

(defun underline (&rest elements)
  (apply #'element "underline" elements))

(defun noindent (&rest elements)
  (apply #'element "noindent" elements))

(defun uppercase (&rest elements)
  (apply #'element "uppercase" elements))

(defun graphicspath (&rest paths)
  "Return a graphicspath element with provided `paths' list.

  Exemples:
  (graphicspath \"path/to/images/\")
  ;; => \"\\graphicspath{{path/to/images}}\"
  (graphicspath \"./path-1/\" \"./path-2/\")
  ;; => \"\\graphicspath{{path-1}{./path-2}}\""
  (apply #'element
         "graphicspath"
         (mapcar #'(lambda (path) (surround-string "{" "}" path))
                 paths)))

(defun includegraphics (graphics-name &key width height scale angle)
  (let ((options (format-options (list 'width width
                                       'height height
                                       'scale scale
                                       'angle angle))))
    (element (concat "includegraphics" options) graphics-name)))

(defun figure (position &rest elements)
  (apply #'begin-end (list "figure" nil (concat "[" position "]")) elements))

(defun caption (&rest elements)
  (apply #'element "caption" elements))
