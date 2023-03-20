#lang info
(define collection 'multi)
(define version    "1.0")
(define deps '("draw-lib"
               "slideshow-lib"
               "web-server-lib"
               "base" "pict"
               ("poppler-x86-64-macosx" #:platform "x86_64-macosx")
               ("poppler-i386-macosx"   #:platform "i386-macosx")
               ("poppler-win32-x86-64"  #:platform "win32\\x86_64")
               ("poppler-win32-i386"    #:platform "win32\\i386")))

(define build-deps '("at-exp-lib" "rackunit-lib" "scribble-lib"
                     "racket-doc" "draw-doc" "pict-doc"))
