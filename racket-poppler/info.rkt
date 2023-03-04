#lang setup/infotab
(define name "racket-poppler")
(define blurb
  '("Read, render and search pdf files. Pdfs can be rendered to picts and bitmaps. You can also gather text and layout information. As a bonus render-tex will render TeX into picts."))
(define primary-file "main.rkt")
(define categories '(media io))
(define repositories '("4.x"))
; (define scribblings '(("doc/main.scrbl" ())))
; (define scribblings '(("doc/main.scrbl" ())))
(define release-notes '((p "Initial release")))
(define omit-compile-paths '("racket-poppler/examples/" "examples/"))
