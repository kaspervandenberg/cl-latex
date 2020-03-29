;;;; ========================================================================================== ;;;;
;;;; cl-tex.lisp                                                                                ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:tex)

(defparameter *default-document-layout*
  "\\usepackage{hyperref}~%\\usepackage[T1]{fontenc}~%~%\\begin{document}~%~%%cl-latex%~%~%\\end{document}"
  "Default LaTeX layout used if no layout was provided by the user.")

(defclass latex-document ()
  ((layout         :initform nil
				   :initarg :layout
				   :accessor layout
				   :documentation "String containing a latex layout.")
   
   (layout-file    :initform nil
				   :initarg :layout-file
				   :accessor layout-file
				   :documentation "Path to a file containing a latex layout.")
   
   (documentclass  :initform '(:class :article)
				   :initarg :documentclass
				   :accessor documentclass)
   
   (packages       :initform nil
				   :initarg :packages
				   :accessor packages)
   
   (preamble       :initform nil
				   :initarg :preamble
				   :accessor preamble)
   
   (body           :initform nil
				   :initarg :body
				   :accessor body
				   :documentation "Body of the LaTeX document."))
  
  (:documentation "Hold information about a LaTeX document."))

(defmethod initialize-instance :after ((doc latex-document) &key)
  (let ((layout-file (layout-file doc)))
	(when (empty? (layout doc))
	  (if (not-empty? layout-file)
		  (setf (layout doc) (load-layout-file layout-file))
		  (setf (layout doc) *default-document-layout*))))
  
  :documentation "If no layout was provided, initialize it with a default layout.")

(defun load-layout-file (file-path)
  "Load a LaTeX layout file from `file-path'."
  (with-open-file (stream file-path)
	(let ((contents (make-string (file-length stream))))
	  (read-sequence contents stream)
	  contents)))

(defun make-latex (&key (layout nil)
							  (layout-file nil)
							  (documentclass '(:class :article))
							  (packages nil)
							  (preamble nil)
							  (body nil))
  "Return a `latex-document' instance with provided params."
  (make-instance 'latex-document
				 :layout layout
				 :layout-file layout-file
                 :documentclass documentclass
                 :packages packages
                 :preamble preamble
                 :body body))

(defmethod to-string ((latex-doc latex-document))
  "Return a `latex-document' formated as a LaTeX document string."
  (let ((header-string (format
																								nil
																								(concat-as-lines (write-documentclass (documentclass latex-doc))
																																									(write-packages (packages latex-doc))
																																									(write-preamble (preamble latex-doc))))))
				(concat-as-string header-string (format nil (insert-body latex-doc)) (format nil "~%"))))

(defmethod insert-body ((latex-doc latex-document))
  "Insert `latex-doc''s body in the layout.
   The point of insertion in the layout is defined by the string `%cl-latex%'."
  (replace-string "%cl-latex%"
				  (format nil (apply #'concat-as-lines (body latex-doc)))
				  (layout latex-doc)))

(defun replace-string (old new string)
  "Return a string with `old' replaced by `new' in `string'."
  (let ((old-location (search old string)))
				(if old-location
								(concat-as-string (subseq string 0 old-location)
																										new
																										(subseq string (+ old-location (length old))))
								(concat-as-string string new)))) ; If old is not found, concat new and string

;; (defun test1 ()
;;   (latex :documentclass (:class :article :options "12pt")
;;          :packages ((:name "crimson")
;;                     (:name "inputenc" :options "utf8")
;;                     (:name "float")
;;                     (:name "enumitem")
;;                     (:name "wrapfig"))
		 
;;          :preamble ("\\setlist[itemize]{label=\textbullet}"
;;                     (setlength "\\parskip" "1em"))
		 
;;          :body (begin-end "document" "~%" (section* "Menhirs en Bretagne")
;; 						 "This is some text~%")))

;; (defun test2 ()
;;   (let ((ltx (make-latex-document :documentclass '(:class :article :options "12pt")
;; 								  :packages '((:name "crimson")
;; 											  (:name "inputenc" :options "utf8")
;; 											  (:name "float")
;; 											  (:name "enumitem")
;; 											  (:name "wrapfig"))

;; 								  :preamble (list "\\setlist[itemize]{label=\textbullet}"
;; 												  (setlength "\\parskip" "1em")
;; 												  (graphicspath "./img/"))
;; 								  :body (list (section* "Menhirs en Bretagne")
;; 											  "This is some text"))))
;;     (to-string ltx)))

(defvar *document-classes*
  '(:article :ieeetran :proc :report :book :slides :memoir :letter :beamer)
  "Generic document classes that come builtin with LaTeX.")

(defmacro latex (&key documentclass packages preamble body)
  "Return a LaTeX document string."
  `(format nil (concat-as-lines (write-documentclass ',documentclass)
                                (write-packages ',packages)
                                (write-preamble ',preamble)
                                ,body)))

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

(defun write-preamble (preamble)
  (apply #'concat-as-lines
         (loop :for item :in preamble
            :if (listp item)
            :collect (apply #'funcall item)
            :else :collect item)))

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

(defun empty? (string)
  "Return t if `string' is empty or nil, return nil otherwise."
  (zerop (length string)))

(defun not-empty? (string)
  "Return t if `string' is not empty, return nil otherwise."
  (not (empty? string)))

(defun concat (&rest strings)
  "Concatenate `strings' into a single string."
  (apply #'concatenate 'string strings))

(defun concat-as-string (&rest args)
  "Concatenate `args' as strings into a single string. If any of the
   arguments is not a string, it is converted with `write-to-string'."
  (let ((concat-strs
         (reduce
          #'(lambda (arg1 arg2)
              (concat
               (when arg1
                 (if (stringp arg1)
                     arg1
                     (write-to-string arg1)))
               (when arg2
                 (if (stringp arg2)
                     arg2
                     (write-to-string arg2)))))
          args)))
    (or concat-strs "")))

(defun concat-as-lines (&rest args)
  "Concatenate `args' separating each other with a newline character."
  (let ((concatenated-string
										(reduce
											(lambda (arg1 arg2)
													(if (or (null arg1)
																					(null arg2)
																					(string-equal arg1 "~%")
																					(string-equal arg2 "~%"))
																	(concat-as-string arg1 arg2)
																	(concat-as-string arg1 "~%" arg2)))
											args)))
				(if (not-empty? concatenated-string)
								(concat-as-string concatenated-string "~%") ; Add linebreak at the end of the final string
								"")))

(defun surround-string (str1 str2 &rest rest)
  "Surround strings from `rest' with `str1' and `str2'."
  (concat str1 (reduce #'concat rest) str2))

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
