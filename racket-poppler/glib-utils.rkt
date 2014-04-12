#lang racket
(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define
         racket/draw/unsafe/glib)

(define-cstruct _GList
  ([data (_or-null _pointer)]
   [next _GList-pointer/null]
   [prev _GList-pointer/null]))
(define GList* _GList-pointer/null)

; In C the empty _GList is represented as NULL, in Racket as #f.
(define (gempty? g) (not g))
; g_list_first returns the first element
(define-glib g_list_first (_fun GList* -> (_or-null _pointer)))

; g_list_next returns the next link
(define (g_list_next gl) (and gl (GList-next gl)))

; g_list_delete_link removes and frees the link (the element is not freed)
(define-glib g_list_delete_link (_fun GList* GList* -> GList*))

; glist->list/free : (or glist #f) -> list
;   return a list with the element of the glist,
;   and free all links of the glist.
(define (glist->list/free gl)
  (displayln (list 'gl gl))
  (if (empty-glist? gl) 
      '()
      (cons (g_list_first gl)
            (let ([next (g_list_next gl)]
                  [rest (g_list_delete_link gl gl)])
              (displayln (list 'rest rest))
              (glist->list/free next)))))