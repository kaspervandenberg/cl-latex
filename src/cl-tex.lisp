;;;; ========================================================================================== ;;;;
;;;; cl-tex.lisp                                                                                ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:tex)

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

(defun test1 ()
  (latex :documentclass (:class :article :options "12pt")

         :packages ((:name "crimson")
                    (:name "inputenc" :options "utf8"))

         :header ("\setlist[itemize]{label=\textbullet}"
                  (setlength "\parskip" "1em"))

         :document (document
                    (section* "Menhirs en Bretagne")
                    "This is some text")))

(defvar *document-classes*
  '(:article :ieeetran :proc :report :book :slides :memoir :letter :beamer)
  "Generic document classes that come builtin with LaTeX.")

(defmacro latex (&key documentclass packages header document)
  `(concat-as-lines (write-documentclass ',documentclass)
                    (write-packages ',packages)
                    (write-header ',header)
                    ,document))

(defun write-documentclass (documentclass)
  (let ((class (getf documentclass :class))
        (options (getf documentclass :options)))
    (assert (position class *document-classes*))
    (element (list "documentclass" options)
             (string-downcase (format nil "~a" class)))))

(defun write-packages (packages)
  (print packages)
  "packages ")

(defun write-header (header)
  (print header)
  "header ")

(defvar linebreak "\\linebreak~%"
  "Default linebreak sintax in Latex")

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
        (arg2 (when (listp args) (cadr args))))
    (assert arg1)
    (surround-string (concat "\\"
                             arg1
                             (when arg2 (concat "[" arg2 "]"))
                             "{")
                     "}"
                     (reduce #'concat-as-string rest))))

(defun begin-end (args &rest elements)
  (let ((arg1 (if (listp args) (car args) args))
        (arg2 (when (listp args) (cadr args))))
    (assert arg1)
    (concat (element "begin" arg1)
            (when arg2 (concat "{" arg2 "}"))
            "~%"
            (apply #'concat-as-lines elements)
            (element "end" arg1))))

(defun document (&rest elements)
  (apply #'begin-end "document" elements))

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

(defun noindent (&rest elements)
  (apply #'element "noindent" elements))

(defun uppercase (&rest elements)
  (apply #'element "uppercase" elements))
