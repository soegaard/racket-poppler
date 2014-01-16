#lang racket
(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define
         racket/draw/unsafe/glib)

(define-ffi-definer define-poppler (ffi-lib "libpoppler-glib"))

(define-cstruct _GList
  ([data _racket]
   [next _GList-pointer/null]
   [prev _GList-pointer/null]))
(define (gdata gl) (and gl (GList-data (ptr-ref gl _GList)))) 
(define (gnext gl) (and gl (GList-next (ptr-ref gl _GList))))
(define (gprev gl) (and gl (GList-prev (ptr-ref gl _GList))))
(define _GList* _GList-pointer)
(define _GList*/null _GList-pointer/null)

; In C the empty _GList is represented as null, in Racket as #f.
; In other words, we must use _GList*/null  for functions that
; return pointers to GLists that might be null.

(define (gempty? gl) (not gl)) ; is the glist empty?
; g_list_first returns the first *link*
(define-glib g_list_first (_fun _GList* -> _GList*/null))
; gfirst returns the first element (or #f for an empty glist)
(define (gfirst gl) (and gl (g_list_first gl)))
; g_list_delete_link removes and frees the link (the element is not freed)
; (define-glib g_list_delete_link (_fun _GList* _GList* -> _GList*/null))
; (define (gdelete gl1 gl2) (if (and gl1 gl2) (g_list_delete_link gl1 gl2) #f))
; (define-glib g_list_free_full (_fun _GList* _GList* -> _GList*/null))
(define-glib g_list_free (_fun _GList* -> _void))
(define (glistfree gl*) (and gl* (g_list_free gl*)))

(define-glib g_list_length   (_fun _GList*       -> _uint))
(define (glength g) (if g (g_list_length g) 0))

; g_list_prepend prepends an object d to the list xs, the new list head is returned
(define-glib g_list_prepend (_fun _GList*/null _racket -> _GList*/null))
(define (gprepend gl* d) (g_list_prepend gl* d))

(let ([xs (gprepend (gprepend (gprepend #f (list 1)) (list 2)) (list 3))])
  (displayln (gdata (gfirst xs)))
  (displayln (gdata (gnext xs)))
  (displayln (gdata (gnext (gnext xs))))
  (displayln (gdata (gnext (gnext (gnext xs)))))
  (glistfree xs)
  )
  





  


