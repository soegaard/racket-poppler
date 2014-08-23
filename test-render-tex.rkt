#lang racket
(require racket-poppler/render-tex pict)

;;; This demonstrates how to use "render-tex.rkt".

(latex-debug? #t)  ; #t turns on useful debug information
; turn this on to see paths and errors.

; the location of pdflatex on OS X with TeXLive installed
(latex-path "/usr/local/texlive/2013/bin/universal-darwin/pdflatex") 

; make a Racket pict from a piece of TeX
(define p (latex->pict "$\\sqrt{x^2+y^2}$"))

(pict->bitmap p)

(define beside hc-append)
(define above  vc-append)

(pict->bitmap  ; pict->bitmap is need in DrRacket 
 ;               DrRacket won't display picts that draw directly
 ;               to the Cairo drawing context
 
 ; This shows that the pict can be used as a normal pict.
 (above (beside (rotate p (- pi (/ pi 3))) (rotate p    (/ pi 3)))
        (beside (rotate p (+ pi (/ pi 3))) (rotate p (- (/ pi 3))))))

(pict->bitmap p)

(define (tex s) (pict->bitmap (inset (latex->pict s) 4)))
