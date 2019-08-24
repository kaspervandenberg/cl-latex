;;;; ========================================================================================== ;;;;
;;;; cl-tex.lisp                                                                                ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:tex)

#|
(deflatex simple_document
    :documentclass (:article "12pt")
    :packages '(("crimson")
                ("inputenc" :args '("utf8"))
                ("fontenc" :args '("T1"))
                ("natbib" :args '("super"))
                ("babel" :args '("spanish"))
                ("hyperref")
                ("geometry")
                ("titlesec")
                ("enumitem" :args '("shortlabels"))
                ("nowidows" :args '("all")))

    :header '("\setlist[itemize]{label=\textbullet}"
              "\geometry{a4paper, margin=1in}"
              (setlength "\parskip" "1em")
              (renewcommand "tiny" "normalsize")
              "\titleformat{\section}[block]{\bfseries\filcenter}{}{1em}{}"
              "\urlstyle{same}")

    :document '(maketitle
                (section* "Menhirs en Bretagne")
                "This is some text"))

(to-string simple_document)
|#

(defun test1 ()
  (format t
          (latex :documentclass (:class :article :options "12pt")
                 :packages ((:name "crimson")
                            (:name "inputenc" :options "utf8")
                            (:name "graphicx")
                            (:name "float")
                            (:name "enumitem")
                            (:name "wrapfig"))
                 :preamble ("\\setlist[itemize]{label=\textbullet}"
                            (setlength "\\parskip" "1em")
                            (graphicspath "./img/"))
                 :document (document
                            (section* "Menhirs en Bretagne")
                            "This is some text"
                            linebreak
                            (figure "H"
                                    (includegraphics "mad1" :scale "0.8")
                                    (caption "This is a caption."))))))

(defvar *document-classes*
  '(:article :ieeetran :proc :report :book :slides :memoir :letter :beamer)
  "Generic document classes that come builtin with LaTeX.")

(defmacro latex (&key documentclass packages preamble document)
  `(concat-as-lines (write-documentclass ',documentclass)
                    (write-packages ',packages)
                    (write-header ',preamble)
                    ,document))

(defun write-documentclass (documentclass)
  (let ((class (getf documentclass :class))
        (options (getf documentclass :options)))
    (assert (position class *document-classes*))
    (element (list "documentclass" (format-options options))
             (string-downcase (format nil "~a" class)))))

(defun write-packages (packages)
  (apply #'concat-as-lines
         (loop :for package :in packages
            :collect (element
                      (list "usepackage" (format-options (getf package :options)))
                      (getf package :name)))))

(defun write-header (header)
  (apply #'concat-as-lines
         (loop :for item :in header
            :if (listp item)
            :collect (apply #'funcall item)
            :else :collect item)))

(defvar linebreak "\\linebreak"
  "Default linebreak sintax in Latex")

(defvar newline "\\newline")
(defvar centering "\\centering")
(defvar columnsep "\\columnsep")
(defvar columnwidth "\\columnwidth")
(defvar linewidth "\\linewidth")
(defvar paperwidth "\\paperwidth")
(defvar paperheight "\\paperheight")
(defvar textwidth "\\textwidth")
(defvar textheight "\\textheight")
(defvar unitleght "\\unitleght")

(defun concat (&rest strings)
  "Concatenate `strings' into a single string."
  (apply #'concatenate 'string strings))

(defun concat-as-string (&rest args)
  "Concatenate `args' as strings into a single string. If any of the
   arguments is not a string, it is converted with `write-to-string'."
  (reduce
   #'(lambda (arg1 arg2)
       (concat
        (if (stringp arg1)
            arg1
            (write-to-string arg1))
        (if (stringp arg2)
            arg2
            (write-to-string arg2))))
   args))

(defun concat-as-lines (&rest args)
  "Concatenate `args' separating each other with a newline character."
  (concat (reduce
           #'(lambda (arg1 arg2) (concat-as-string arg1 "~%" arg2))
           args)
          "~%"))

(defun surround-string (str1 str2 &rest rest)
  "Surround strings from `rest' with `str1' and `str2'."
  (concat str1 (reduce #'concat rest) str2))

(defun element (args &rest rest)
  "Facilitate the creation of a LaTeX element string."
  (let ((arg1 (if (listp args) (car args) args))
        (arg2 (when (listp args) (cadr args)))
        (arg3 (or (when (listp args) (caddr args)) "[]")))
    (assert arg1)
    (surround-string (concat "\\"
                             arg1
                             (when arg2 (concat (subseq arg3 0 1)
                                                arg2
                                                (subseq arg3 1 nil)))
                             "{")
                     "}"
                     (reduce #'concat-as-string rest))))

(defun element (args &rest rest)
  (let ((arg1 (if (listp args) (car args) args))
        (arg2 (when (listp args) (cadr args))))
    (surround-string
     (concat "\\" arg1 arg2 "{")
     "}"
     (reduce #'concat-as-string rest))))

(defun format-options (options)
  "Receive a list of key and values, and return a string with formated options
  for LaTeX.

  Example:
  (format-options '(width 2 height 4))
  ;; => \"[width=2,height=4]\""
  (if (listp options)
      (let ((options-str
             (loop :for v := 1 :then (+ v 2)
                :for k := 0 :then (+ k 2)
                :while (< v (length options))
                :when (elt options v)
                :collect (concat-as-string (string-downcase (elt options k))
                                           "="
                                           (elt options v)))))

        (when options-str (concat "["
                                  (reduce #'(lambda (s1 s2) (concat s1 "," s2))
                                          options-str)
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

(defun document (&rest elements)
  (apply #'begin-end "document" elements))

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
