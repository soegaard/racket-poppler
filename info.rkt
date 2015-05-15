#lang info
(define collection 'multi)
(define version    "1")
(define deps '("draw-lib"
               "slideshow-lib"
               "web-server-lib"
               "base" "pict"))

(define build-deps '("at-exp-lib" "rackunit-lib" "scribble-lib"
                     "racket-doc" "draw-doc" "pict-doc"))
