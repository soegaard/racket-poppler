#lang racket/base
;;;
;;; Convert LaTeX expressions (strings) into picts.
;;;

;; This converter uses pdflatex to convert the LaTeX expressions
;; to pdf, then uses the Poppler bindings to turn it into a pict.
;; The pict than then be converted into a bitmap.

;; Goal: Be compatible with Neil&Jay's slideshow-latex

;; Debugging tips:
;;   Open latest pdf with:
;;      ls -t *pdf | head -n1 | xargs open
;;   Rerun latest latex command with:
;;      ls -t *tex | head -n1 | xargs pdflatex
;;   See latest latex log with:
;;      ls -t *log | head -n1 | xargs less
;;   Or make print the log automtically, if an error occurs:
;;      (latex-debug? #t)

;; Note:
;;  Computer Modern Fonts for mac:
;;     https://apple.stackexchange.com/questions/117946/computer-modern-font-for-osx
;;  Useful when comparing baselines between pict made by (text ...)
;;  and a pict made by latex->pict.

;; Note:
;;  The meaning of height and depth in Tex is different from
;;  the meaning of height in a pict. See extract-sizes-from-latex-log.

;; Note:
;;  To compare the orginal baseline in TeX with the baseline produced by
;;  latex->pict, it can be useful to make TeX draw a baseline.
;;    (current-preamble
;;       @~a{
;;          \usepackage{mathtools}
;;          \usepackage{amssymb}
;;          \usepackage{xcolor}
;;          \newcommand*\drawbaseline[2][orange]
;;          {\begingroup\sbox0{$\displaystyle#2$}\mathrlap{\color{#1}\rule{\wd0}{.1pt}}\endgroup#2}
;;          })
;;  One can now write \drawbaseline{...} around a part of a math formula,
;;  to see the baseline.

;; Note:
;;  The option "lyx" is used with the package "preview" in order to see
;;  the height, depth and width of the snippet.

;; Note:
;;   There are three different latex modes: normal text mode, math mode and display math mode.
;;   Should there be three different converters?

(require (for-syntax racket/base)
         racket/system racket/string racket/format
         racket/port racket/list file/md5 racket/match
         pict racket-poppler)

(provide latex->pict latex->bitmap
         latex-preamble add-preamble latex-debug?
         latex-path
         (struct-out exn:latex)
         set-latex-cache-path
         tidy-latex-cache
         setup-local-latex-cache
         last-extracted-baseline-fraction)

(define used-files   (make-hash))
(define cached-picts (make-hash))


;; Document contents
(define latex-preamble (make-parameter ""))

(define (add-preamble . latex-strs)
  (latex-preamble (string-join (cons (latex-preamble) latex-strs) "\n")))

;; Spam the console with debugging info?
(define latex-debug? (make-parameter #f)) ; todo : set to #f

(define (find-executable name [defaultdir #f])
  (set! defaultdir
        (or defaultdir
            (case (system-type)
              [(macosx) "/Library/TeX/texbin"]
              [else     "/usr/bin"])))
  (cond [(find-executable-path name) => (λ (p) p)]
        [(find-executable-path (string-append name ".exe")) => (λ (p) p)]
        [else  (build-path defaultdir name)]))

;; Default paths
(define latex-path  (make-parameter (find-executable  "pdflatex")))
(define dvipng-path (make-parameter (find-executable "dvipng")))
(define cache-path  (make-parameter (find-system-path 'temp-dir)))

;; Exceptions raised
(struct exn:latex exn:fail (message))
;(struct exn:dvipng exn:fail ())

;; base+ext : path? string? -> path?
(define (base+ext file-base ext)
  (build-path (string-append (path->string file-base) ext)))

;; text-file->string : path? -> string?
(define (text-file->string file)
  (call-with-input-file file #:mode 'text port->string))

;; string->text-file : string? path? -> void?
(define (string->text-file str file)
  (with-output-to-file file #:mode 'text #:exists 'truncate
    (lambda () (printf "~a" str))))

;; text-file->string-list : path? -> (listof string?)
(define (text-file->string-list file)
  (call-with-input-file file #:mode 'text port->lines))

;; Example error:
;; ./latex125249806.tex:18: Undefined control sequence.

;; parse-latex-log : path? -> string?
;; Returns the first error message and context string found in the log
(define (parse-latex-log base-file)
  (define error-regexp
    (regexp (format ".*~a.*:[0-9]+:" (path->string base-file))))

  (let loop ([lines  (text-file->string-list (base+ext base-file ".log"))])
    (cond [(empty? lines)  "unknown"]
          [(regexp-match error-regexp (first lines))
           (let* ([info  (regexp-split #rx":" (first lines))]
                  [msg  (string-join (drop info 2) ":")]
                  [ctx  (string-join (take (rest lines) 2) "\n")])
             (format "~a ~a" msg ctx))]
          [else  (loop (rest lines))])))

; extract-sizes-from-latex-log : path? -> string?
;   the "lyx" option for the "preview" package makes
;   pdflatex write a line:
; Preview: Snippet 1 422343 127431 663780
;                  n height depth  width
; giving TeX sizes height, depth and width.
; Note that TeX used height and depth differently from picts.
;    tex-height + tex-depth                       = tex-total-height
;   pict-ascent + pict-middle+pict + pict-descent = pict-height
; The text-height is therefore the placement of the baseline.
; In order to avoid unit conversions, we calculate fraction
;     text-height/text-total-height,
; and multiply with pict-height to the placement of the pict baseline.

; The package "preview" can be used to extract multiple snippets at a
; time, so the number n is simply a counter used to identify which snippet
; the size belong to.

(define (extract-sizes-from-latex-log base-file)
  (define size-regexp (regexp ".*Preview: Snippet ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+).*"))
  ; (define test-line         "Preview: Snippet 1 422343 127431 663780")
  (define (match->fraction m)
    (match-define (list line counter height depth width) m)
    (match-define (list h d w) (map string->number (list height depth width)))
    ; above and below the actual box there is a bit of space.
    ; the number from tightpage is added both above and below.
    (define tp 32891) ; "Preview: Tightpage -32891 -32891 32891 32891"
    ;(displayln "Extract: ")
    ;(displayln (~a 'h: h 'd: d 'w: w))
    ;(displayln (list h d w))  ; xxx
    (if (< (+ h d) 0.1)
        0.5                 ; the fallback is baseline at the middle
        (min 1 (max 0 (/ (+ tp h)               ; position of baseline
                         (+ h d (* 2 tp)))))))  ; total height

  (with-input-from-file (base+ext base-file ".log")
    (λ ()
      (for/or ([line (in-lines)])
        (cond [(regexp-match size-regexp line) => match->fraction]
              [else #f])))))

;; compile-latex : path? -> void?
(define (compile-latex file-base)
  (define tex-file (base+ext file-base ".tex"))
  (define pdf-file (base+ext file-base ".pdf"))

  (define latex-args
    (list "-interaction=batchmode" "-file-line-error" "-halt-on-error"
          (path->string tex-file)))

  (define pdflatex-args
    (list "-interaction=batchmode" "-file-line-error" "-halt-on-error"
          (path->string tex-file)))

  (when (latex-debug?)
    (printf "INFO: ~a ~a~n" (latex-path) (string-join latex-args " ")))

  (when (not (zero? (parameterize ([current-output-port (open-output-nowhere)])
                      (apply system*/exit-code (latex-path) latex-args))))
    (when (latex-debug?)
      (printf "ERROR: latex error, here's the log:~n")
      (printf "~a~n" (text-file->string (base+ext file-base ".log"))))
    (raise (exn:latex "latex failed" (current-continuation-marks)
                      (parse-latex-log file-base)))))

;; latex-doc->file-base : string? -> string?
(define (latex-doc->file-base doc-str)
  (build-path
   (~a "latex2pdf-" (bytes->string/latin-1
                     (call-with-input-string doc-str md5)))))

;; ensure-pdf : string? -> void?
(define (ensure-pdf file-base doc-str)
  (define tex-file (base+ext file-base ".tex"))
  (define pdf-file (base+ext file-base ".pdf"))

  (unless (file-exists? pdf-file)
    (when (latex-debug?) (printf "INFO: generating ~a~n" pdf-file))
    (string->text-file doc-str tex-file)
    (compile-latex file-base))

  (hash-set! used-files pdf-file #t))

(define (brackets x)
  (match x
    ["" ""]
    [(? string? x) (string-append "[" x "]")]
    [_ (error 'brackets (~a "expected a string, got: " x))]))

(define (format-options options format-option)
  (brackets
   (string-append*
    (add-between (filter values (map format-option options))
                 ","))))

(define (format-document-class-options opts)
  (define (format-option opt)
    (match opt
      [#f                    #f]
      [(and (or 10 11 12) n) (~a n "pt")]
      [(? integer? n)        (if (< n 10) "10pt" "12pt")]
      [(? string? s)         s]
      [(? symbol? s)         (~a s)]
      [else                  (error 'format-document-class-option (~a "got: " opt))]))
  (format-options opts format-option))

(define (format-preview-options opts)
  (define (format-option opt)
    (match opt
      [(? string? s)         s]
      [(? symbol? s)         (~a s)]
      [else                  (error 'format-preview-option (~a "got: " opt))]))
  (format-options opts format-option))

;; latex->latex-doc : string? -> string?
(define (latex->latex-doc
         latex-str
         #:preview-options        [preview-options '()]
         #:document-class-options [doc-options     '()]
         #:preamble               [preamble        (or (latex-preamble) #f)])
  (define doc-opts      (format-document-class-options doc-options))
  (define preview-opts  (format-preview-options
                         (append '(active tightpage lyx pdftex) preview-options)))
  (define the-preamble  (or preamble "\\usepackage{amsmath}"))
  (string-append "\\documentclass" doc-opts     "{standalone}\n"
                 "\\usepackage"    preview-opts "{preview}\n"
                 (~a the-preamble "\n")
                 "\\begin{document}\n"
                 (~a latex-str "\n")
                 "\\end{document}\n"))

;; "\\usepackage[active,tightpage,pdftex]{preview}\n"
;; This means use the "preview" packages with the options:
;;    active    - actually use preview (otherwise preview is ignored)
;;    tightpage - option is to be used with the option pdftex
;;    pdftex    - assume PDFTeX is the output driver (affects tightpage)
;; "\\usepackage{amsmath}\n"   ; make amsmath available in math

(define last-extracted-baseline-fraction #f)
(define (latex->pict latex-str
                     #:document-class-options     [doc-options     '()]
                     #:preview-options            [preview-options '()]
                     #:preamble                   preamble
                     #:extract-baseline-fraction? [extract? #f])
  ; Construct the LaTeX document
  (define doc-str   (latex->latex-doc
                     latex-str
                     #:document-class-options     doc-options
                     #:preview-options            preview-options
                     #:preamble                   preamble))
  ; Construct filenames for pdf and log
  (define file-base (latex-doc->file-base doc-str))
  (define pdf-file  (base+ext file-base ".pdf"))
  (define log-file  (base+ext file-base ".log"))
  ;
  (hash-ref!
   cached-picts file-base
   (λ () (parameterize ([current-directory  (cache-path)])
           (ensure-pdf file-base doc-str)
           (when (latex-debug?)
             (printf "INFO: loading ~a~nfolder: ~a~n" pdf-file (cache-path)))
           (define p (page->pict (pdf-page (open-pdf pdf-file) 0)))
           (define f (and extract?
                          (file-exists? log-file)
                          (extract-sizes-from-latex-log file-base)))
           (set! last-extracted-baseline-fraction f)
           ; (displayln (list 'f f))
           (when f ; adjust the baseline
             ; (displayln "lifting!")
             (set! p (lift-above-baseline p (* (- f 1) (pict-height p)))))
           p))))

;; latex->bitmap : string? -> bitmap%
(define (latex->bitmap latex-str #:preamble [preamble (or (latex-preamble) "")])
  (pict->bitmap (latex->pict latex-str #:preamble preamble)))


(define (set-latex-cache-path cpath)
  (when (file-exists? cpath)
    (error 'set-cache "~e exists as a regular file" cpath))
  (when (not (directory-exists? cpath))
    (make-directory cpath))
  (when (latex-debug?) (printf "INFO: latex compile path set to ~a~n" cpath))
  (cache-path cpath))

(define (tidy-latex-cache)
  ; (set! cached-picts (make-hash))
  (for ([file  (directory-list (cache-path))])
    (when (and (regexp-match #rx"^latex2pdf-" (path->string file))
               (not (hash-has-key? used-files file)))
      (when (latex-debug?) (printf "INFO: deleting unused file ~a~n" file))
      (delete-file (build-path (cache-path) file)))))

(define (setup-local-cache stx)
  (let ([src  (syntax-source stx)])
    (if (path? src)
        (let-values ([(base-dir modname _) (split-path src)])
          (set-latex-cache-path (path->directory-path
                                 (build-path base-dir (format "~a-latex" modname)))))
        (printf "WARNING: not a path: ~a~n" src))))

(define-syntax (setup-local-latex-cache stx)
  (syntax-case stx ()
    [(local-name)  #'(setup-local-cache #'local-name)]))

; Example path (OS X using TeXLive)
; (latex-path "/usr/local/texlive/2013/bin/universal-darwin/pdflatex")
; (latex-debug? #t)
; (cache-path)
; (latex-path "/Library/TeX/texbin/pdflatex")
; (pict->bitmap (scale (latex->pict "Foobar") 8))
; (pict->bitmap (scale (latex->pict "$\\sqrt{x^2+y^2 }$") 8))
; (pict->bitmap (rotate (latex->pict "$\\sqrt{x^2+y^2 }$")  (/ 3.1415 3)))
