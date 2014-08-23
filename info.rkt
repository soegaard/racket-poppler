#lang info
(define collection 'multi)
(define version    "1")
(define deps '("base" "pict"))

(define build-deps '("at-exp-lib" "rackunit-lib" "scribble-lib"
                     "racket-doc" "draw-doc" "pict-doc"))

(define copy-shared-files (list "poppler-libs"))
