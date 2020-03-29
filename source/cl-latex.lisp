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

(defmethod to-string ((latex-doc latex-document))
  "Return a `latex-document' formated as a LaTeX document string."
  (if (layout-file latex-doc)
      (insert-body latex-doc)
      (let ((header-string
              (format nil
                      (concat-as-lines (write-documentclass (documentclass latex-doc))
                                       (write-packages (packages latex-doc))
                                       (write-preamble (preamble latex-doc))))))
        (concat-as-string header-string (format nil (insert-body latex-doc)) (format nil "~%")))))

(defmethod insert-body ((latex-doc latex-document))
  "Insert `latex-doc''s body in the layout.
   The point of insertion in the layout is defined by the string `%cl-latex%'."
  (replace-string "%cl-latex%"
                  (format nil (apply #'concat-as-lines (body latex-doc)))
                  (layout latex-doc)))

(defmethod generate-pdf (filename (latex-doc latex-document))
  "Uses latexmk or pdflatex to generate a pdf from the object."
  (let ((latexmk-exists? (/= 1 (multiple-value-last
                                (uiop:run-program "which latexmk"
                                                  :output :string
                                                  :ignore-error-status t
                                                  :error-output t))))
        
        (pdflatex-exists? (/= 1 (multiple-value-last
                                 (uiop:run-program "which pdflatex"
                                                   :output :string
                                                   :ignore-error-status t
                                                   :error-output t)))))
    (if (null (or latexmk-exists? pdflatex-exists?))
        (error "ERROR: It was not possible to find latexmk nor pdflatex. Please install one of the two to generate a pdf.~%")
        (let ((latex-string (to-string latex-doc)))
          (if latexmk-exists?
              (generate-pdf-from-string filename latex-string :generator :latexmk)
              (generate-pdf-from-string filename latex-string :generator :pdflatex))))))

(defun generate-pdf-from-string (filename string &key generator)
  (let ((latex-file (concat-as-string filename ".tex")))
    (save-to-file latex-file string)
    (let ((command (concat-as-string
                    (ecase generator
                      (:latexmk "latexmk -pdf ")
                      (:pdflatex "pdflatex "))
                    filename)))
      (uiop:run-program command :output t :error-output t)
      (if (equal generator :latexmk)
          (uiop:run-program (concat-as-string "latexmk -c " filename ".pdf")
                            :output t :error-output t)
          (clean-pdflatex-aux-files filename)))))

(defun clean-pdflatex-aux-files (filename)
  (remove-file (concat-as-string filename ".aux"))
  (remove-file (concat-as-string filename ".log"))
  (remove-file (concat-as-string filename ".out")))

(defun remove-file (filename)
  (uiop:run-program (concat-as-string "rm -f " filename)
                    :output t :error-output t))

(defun save-to-file (filename string)
  (with-open-file (stream filename
                          :direction :output
                          :if-does-not-exist :create)
    (format stream string)))


(defmacro multiple-value-last (form)
  "Get the last element of a function `fn' with multiple return values."
  `(car (reverse (multiple-value-list ,form))))

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

(defmacro latex (&key documentclass packages preamble body)
  "Return a LaTeX document string."
  `(format nil (concat-as-lines (write-documentclass ',documentclass)
                                (write-packages ',packages)
                                (write-preamble ',preamble)
                                ,body)))

(defun write-documentclass (documentclass)
  (when documentclass
    (let ((class (getf documentclass :class))
          (options (getf documentclass :options)))
      (assert (position class *document-classes*))
      (element (list "documentclass" (format-options options))
               (string-downcase (format nil "~a" class))))))

(defun write-packages (packages)
  (when packages
    (apply #'concat-as-lines
           (loop :for package :in packages
                 :collect (element
                           (list "usepackage" (format-options (getf package :options)))
                           (getf package :name))))))

(defun write-preamble (preamble)
  (when preamble
    (apply #'concat-as-lines
           (loop :for item :in preamble
                 :if (listp item)	:collect (apply #'funcall item)
                   :else :collect item))))
