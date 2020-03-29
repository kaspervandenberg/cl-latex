# CL-LATEX

Common Lisp library for LaTeX generation.

## Getting Started

### Usage example
#### Directly generate a LaTeX string

In this first example we use a macro to directly have a LaTeX string output.

```lisp
(latex :body (begin-end "document"
                        (section "Section title")
                        (emph "A short emphasized paragraph.")))
                            
;; => "
;;    \\begin{document}
;;    \\section{Section title}
;;    \\emph{A short emphasized paragraph}
;;    \\end{document}
;;    "
```

#### Generate a pdf from a LaTeX document object

Now, instead of using a macro, we create a `latex-document` with `tex:make-latex`. Note the slight different syntax from the first example when defining the body using `list`.

```lisp
(let ((latex-document (make-latex :layout-file "./layout.tex"
                                  :body (list (section "Section title")
                                              (emph "A short emphasized paragraph.")))))
 (generate-pdf latex-document))
```

#### Generate LaTeX using another LaTeX file as layout

In a file called `layout.tex` write some LaTeX defaults and add `%cl-latex%` where you want the generated LaTeX to be inserted:

```latex
\documentclass[12pt]{article}
\usepackage[T1]{fontenc}

\begin{document}

%cl-latex%

\end{document}
```

Then with the library:

```lisp
(let ((latex-document (make-latex :layout-file "./layout.tex"
                                  :body (list (section "Section title")
                                              (emph "A short emphasized paragraph.")))))
 (to-string latex-document))
 
;; => "\\documentclass[12pt]{article}
;;     \\usepackage[T1]{fontenc}
;;
;;    \\begin{document}
;;
;;    \\section{Section title}
;;    \\emph{A short emphasized paragraph}
;;
;;    \\end{document}
;;    "
```

### Prerequisites

There are no dependencies for the core LaTeX generation.
If you want to generate pdfs, however, you need either [latexmk](https://mg.readthedocs.io/latexmk.html) or [pdflatex](https://linux.die.net/man/1/pdflatex).

### Installing

On UNIX:

```bash
$ git clone https://github.com/stefandevai/cl-latex ~/common-lisp/
```

### Running the tests

The tests are located in `./tests`. There are several unit and integration tests at the moment. You can use them as a reference on how to use the library.

```bash
$ ./run-tests.lisp
```

## Author

* **Stefan Devai** - [stefandevai](https://github.com/stefandevai)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

This library was created as a support for [ibidem](https://github.com/stefandevai/ibidem), a CLI tool that makes it easier to write academic papers with citations.
