#lang racket
(require "ffi.rkt" racket/draw slideshow/pict)

;;; Convenient contracts
(define pdf-doc/c pdf?)
(define path-or-string/c (or/c path? string?))

(provide/contract [pdf-file? (-> path-or-string/c boolean?)])

(provide/contract [open-pdf (->* [(or/c path? string?)]   ; mandatory
                                 [(or/c string? false?)]  ; optionals
                                 (or/c pdf-doc/c #f))])


(provide/contract
 [pdf? (any/c . -> . boolean?)]
 [page? (any/c . -> . boolean?)]
 [rectangle? (any/c . -> . boolean?)]
 #;[unsafe-open-pdf-uri (string? (or/c string? false?) . -> .
                               (or/c PopplerDocumentPointer? false?))]
 ;[open-pdf (or/c path? document?) . -> . PopplerDocumentPointer?]
 [pdf-page (pdf? exact-nonnegative-integer? . -> . (or/c page? #f))]
 [pdf-count-pages (pdf? . -> . exact-nonnegative-integer?)]
 [page-size (page? . -> . (list/c (and/c real? (not/c negative?))
                                  (and/c real? (not/c negative?))))]
 [page-crop-box (page? . -> . rectangle?)]
 [page-text-in-rect (page? (one-of/c 'glyph 'word 'line)
                               (and/c inexact? (not/c negative?)) ; todo accept exact numbers
                               (and/c inexact? (not/c negative?))
                               (and/c inexact? (not/c negative?))
                               (and/c inexact? (not/c negative?))
                               . -> . string?)]
 [page-text (page? . -> . string?)]
 [page-find-text (page? string? . -> . (listof rectangle?))]
 [page-text-layout (page? . -> . (listof rectangle?))]
 [page-text-with-layout (page? . -> . (listof (list/c string? rectangle?)))]
 [page-render-to-cairo! (page? any/c . -> . any/c)]
 [page-render-to-dc! (page? (is-a?/c dc<%>) . -> . any/c)]
 [page->pict   (page? . -> . pict?)]
 [page->bitmap (page? . -> . (is-a?/c bitmap%))]

 [pdf-title    (pdf? . -> . (or/c false? string?))]
 [pdf-author   (pdf? . -> . (or/c false? string?))]
 [pdf-subject  (pdf? . -> . (or/c false? string?))]
 [pdf-keywords (pdf? . -> . (or/c false? string?))]
 [pdf-creator  (pdf? . -> . (or/c false? string?))]
 [pdf-producer (pdf? . -> . (or/c false? string?))]
 [page-label   (page? . -> . (or/c false? string?))])
