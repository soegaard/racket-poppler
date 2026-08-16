#lang info
(define collection 'multi)
(define version    "1.0")
(define deps '("draw-lib"
               "slideshow-lib"
               "web-server-lib"
               "base" "pict"
               ("poppler-aarch64-macosx-2" #:platform "aarch64-macosx")
               ("poppler-x86_64-macosx-2"  #:platform "x86_64-macosx")
               ("poppler-win32-arm64-2"    #:platform "win32\\arm64")
               ("poppler-win32-x86_64-2"   #:platform "win32\\x86_64")))

(define build-deps '("at-exp-lib" "rackunit-lib" "scribble-lib"
                     "racket-doc" "draw-doc" "pict-doc"))
