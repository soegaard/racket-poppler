#lang racket/base
;;;
;;; Convert LaTeX expressions (strings) into picts.
;;;

;; This converter uses pdflatex to convert the LaTeX expressions
;; to pdf, then uses the Poppler bindings to turn it into a pict.
;; The pict than then be converted into a bitmap.

;; Goal: Be compatible with Neil&Jay's slideshow-latex

(require (for-syntax racket/base)
         racket/system 
         racket/string
         racket/format
         racket/port
         racket/list
         file/md5
         pict
         racket-poppler)

(provide latex->pict latex->bitmap
         latex-preamble add-preamble latex-debug?
         latex-path 
         (struct-out exn:latex)
         set-latex-cache-path
         tidy-latex-cache
         setup-local-latex-cache)

(define used-files (make-hash))

(define cached-picts (make-hash))

;; Document contents
(define latex-preamble (make-parameter ""))

(define (add-preamble . latex-strs)
  (latex-preamble (string-join (cons (latex-preamble) latex-strs) "\n")))

;; Spam the console with debugging info?
(define latex-debug? (make-parameter #f)) ; todo : set to #f

(define (find-executable name [defaultdir "/usr/bin"])
  (cond [(find-executable-path name) => (λ (p) p)]
        [(find-executable-path (string-append name ".exe")) => (λ (p) p)]
        [else  (build-path defaultdir name)]))

;; Default paths
(define latex-path (make-parameter (find-executable "latex" "/usr/bin")))
(define dvipng-path (make-parameter (find-executable "dvipng" "/usr/bin")))
(define cache-path (make-parameter (find-system-path 'temp-dir)))

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

;; latex->latex-doc : string? -> string?
(define (latex->latex-doc latex-str)
  (string-append "\\documentclass[tightpage]{standalone}\n"
                 "\\usepackage{amsmath}\n"
                 ; "\\pagestyle{empty}\n" 
                 (~a (latex-preamble) "\n")
                 "\\begin{document}\n"
                 (~a latex-str "\n")
                 "\\end{document}\n"))

(define (latex->pict latex-str)
  (define doc-str (latex->latex-doc latex-str))
  (define file-base (latex-doc->file-base doc-str))
  (define pdf-file (base+ext file-base ".pdf"))  
  (hash-ref!
   cached-picts file-base
   (λ () (parameterize ([current-directory  (cache-path)])
           (ensure-pdf file-base doc-str)
           (when (latex-debug?) (printf "INFO: loading ~a~n" pdf-file))
           (page->pict (pdf-page (open-pdf pdf-file) 0))))))

;; latex->bitmap : string? -> bitmap%
(define (latex->bitmap latex-str)
  (pict->bitmap (latex->pict latex-str)))


(define (set-latex-cache-path cpath)
  (when (file-exists? cpath)
    (error 'set-cache "~e exists as a regular file" cpath))
  (when (not (directory-exists? cpath))
    (make-directory cpath))
  (when (latex-debug?) (printf "INFO: latex compile path set to ~a~n" cpath))
  (cache-path cpath))

(define (tidy-latex-cache)
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

; > (pict->bitmap (rotate (latex->pict "$\\sqrt{x^2+y^2}$")  (/ 3.1415 3)))

