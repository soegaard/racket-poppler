#lang racket
(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define
         (prefix-in gui: racket/gui)
         racket/draw
         racket/draw/unsafe/cairo
         racket/draw/private/local ; needed for 'in-cairo-context'
         racket/draw/unsafe/pango ;; g_object_unref
         racket/draw/unsafe/glib
         (only-in slideshow/pict dc))

(provide (all-defined-out))

(define-cpointer-type _PopplerDocumentPointer)
(define-cpointer-type _PopplerPagePointer)

(define pdf-document?
  (or/c path-string? PopplerDocumentPointer?))
(define pdf-page?
  (or/c pdf-document? PopplerPagePointer?))
(define rectangle?
  (list/c (and/c real? (not/c negative?))
          (and/c real? (not/c negative?))
          (and/c real? (not/c negative?))
          (and/c real? (not/c negative?))))

(define-cstruct _PopplerRectangle
  ([x1 _double] [y1 _double] [x2 _double] [y2 _double]))

;;; GLib Lists
; Docs: https://developer.gnome.org/glib/2.30/glib-Doubly-Linked-Lists.html

(define-cstruct _GList
  ([data (_or-null _pointer)]
   [next _GList-pointer/null]
   [prev _GList-pointer/null]))

; In C the empty _GList is represented as null, in Racket as #f.
(define (gempty? gl) (not gl))
; g_list_first returns the first element
(define-glib g_list_first (_fun _GList -> (_or-null _pointer)))
(define (gfirst gl) (and gl (g_list_first gl)))
; g_list_next returns the next link
(define (gnext gl) (and gl (GList-next gl)))
; g_list_delete_link removes and frees the link (the element is not freed)
(define-glib g_list_delete_link (_fun _GList _GList -> _GList))

; glist->list/free : (or glist #f) -> list
;   return a list with the element of the glist,
;   and free all links of the glist.
(define (glist->list/free gl)
  (displayln (list 'gl gl))
  (if (gempty? gl) 
      '()
      (cons (gfirst gl)
            (let ([next (gnext gl)]
                  #;[rest (g_list_delete_link gl gl)])
              ; (displayln (list 'rest rest))
              (glist->list/free next)))))

; g_list_free deallocates the list (but not its elements)
(define-glib g_list_free     (_fun _GList       -> _void) #:wrap (deallocator))
; g_list_length returns the length (takes time O(n))
(define-glib g_list_length   (_fun _GList       -> _uint))
(define (glength g) (if g (g_list_length g) 0))
; g_list_nth_data returns a pointer to the nth element
(define-glib g_list_nth_data (_fun _GList _uint -> _pointer))


;; O(n^2) but i don't care; i'm just bitter about using GList
(define (glist->list/free! glist ctype)
  (define n (glength glist))
  (begin0
    (for/list ([i (in-range n)])
      (ptr-ref (g_list_nth_data glist i) ctype))
    (unless (= n 0)
      (g_list_free glist))))

;; See: http://comments.gmane.org/gmane.comp.lang.racket.user/11169
(define-ffi-definer define-poppler (ffi-lib "libpoppler-glib"))


; Note: This also defines PopplerRectangle->list!

; PopplerRectangle->list-bottomup : natural rectangle -> rectangle
;  flip the y-coordinates
(define (PopplerRectangle->list-bottomup height rect)
  (define (flip y) (- height y))
  (list (PopplerRectangle-x1 rect) (flip (PopplerRectangle-y2 rect))
        (PopplerRectangle-x2 rect) (flip (PopplerRectangle-y1 rect)))) 

(define (grectangles->list grs)
  (for/list ([r* (in-list (glist->list/free grs))])
    (cast r* _PopplerRectangle)))

(define-poppler open-pdf-uri
  (_fun [uri : _string]
        [password : _string]
        [err : _pointer = #f]
        -> [return : (_or-null _PopplerDocumentPointer)]
        -> (if return return (error "Could not open file " uri)))
  #:c-id poppler_document_new_from_file
  #:wrap (allocator g_object_unref))
;; BUG: What happens when this document is freed but the pages aren't?
;; This can happen, say, if a user of this library keeps a reference
;; to the result of (to-page) but not (to-doc).

;; Try to coerce anything to a document.
;; Nicer function for opening a PDF by filename
(define (to-doc maybe-doc)
  (cond
   [(PopplerDocumentPointer? maybe-doc) maybe-doc]
   [else
    ;; Open it for em
    (define uri
      (string-append "file:"
                     (path->string (path->complete-path maybe-doc))))
    (open-pdf-uri uri #f)]))

(define-poppler pdf-page
  (_fun (maybe-doc index) ::
        [doc-ptr : _PopplerDocumentPointer = (to-doc maybe-doc)]
        [index : _int]
        -> _PopplerPagePointer)
  #:c-id poppler_document_get_page
  #:wrap (allocator g_object_unref))

;; Try to coerce anything to a page. Will pick the first page in the
;; document unless you hand in a page pointer.
(define (to-page maybe-pg)
  ;; maybe-pg: ((or/c pdf-page? pdf-doc?) -> _PopplerPagePointer)
  (cond
   [(PopplerPagePointer? maybe-pg) maybe-pg]
   [else (pdf-page maybe-pg 0)]))

(define-poppler pdf-count-pages
  (_fun (maybe-doc) ::
        [doc-ptr : _PopplerDocumentPointer = (to-doc maybe-doc)]
        -> _int)
  #:c-id poppler_document_get_n_pages)

(define-poppler page-size
  ;; is this in points? i think this might be in points.
  (_fun (maybe-page) ::
        [page-ptr : _PopplerPagePointer = (to-page maybe-page)]
        [width : (_ptr o _double)]
        [height : (_ptr o _double)]
        -> _void
        -> (list width height))
  #:c-id poppler_page_get_size)

(define-poppler page-crop-box
  (_fun (maybe-page) ::
        [page-ptr : _PopplerPagePointer = (to-page maybe-page)]
        [rect : (_ptr o _PopplerRectangle)]
        -> _void
        -> (list (PopplerRectangle-x1 rect)
                 (PopplerRectangle-y1 rect)
                 (PopplerRectangle-x2 rect)
                 (PopplerRectangle-y2 rect)))
  #:c-id poppler_page_get_crop_box)

(define-poppler page-text-in-rect
  (_fun (maybe-page style x1 y1 x2 y2) ::
        [page-ptr : _PopplerPagePointer = (to-page maybe-page)]
        [style : (_enum '(glyph word line))]
        [rect : (_ptr i _PopplerRectangle)
              = (make-PopplerRectangle x1 y1 x2 y2)]
        -> _string)
  #:wrap (allocator g_free)
  #:c-id poppler_page_get_selected_text)

(define-poppler page-text
  (_fun (maybe-page) ::
        [page-ptr : _PopplerPagePointer = (to-page maybe-page)]
        -> _string)
  #:wrap (allocator g_free)
  #:c-id poppler_page_get_text)

(define-poppler page-find-text
  (_fun (maybe-page text) ::
        [page-ptr : _PopplerPagePointer = (to-page maybe-page)]
        [text : _string]
        -> [rglist : _GList]
        ;; poppler returns "PDF coordinates" (Y-axis flipped), boo.
        -> #;(let ([h (second (page-size page-ptr))])
             (define rs (grectangles->list rglist))
             (displayln (list 'rects rs))
             (map (λ(r) (PopplerRectangle->list-bottomup h r)) rs))
        (map (curry PopplerRectangle->list-bottomup
                      (second (page-size page-ptr)))
               (glist->list/free! rglist _PopplerRectangle)))
  #:c-id poppler_page_find_text)

(define-poppler page-text-layout
  (_fun (maybe-page) ::
        [page-ptr : _PopplerPagePointer = (to-page maybe-page)]
        [rects : (_ptr o _pointer)]
        ;; my brain just exploded. ^^ is this right?
        [nrects : (_ptr o _uint)]
        -> [rglist : _bool]
        -> (let ()
             (define rs (map PopplerRectangle->list 
                             (cblock->list rects _PopplerRectangle nrects)))
             ; only call g_free, if there are rectangles to free
             (unless (empty? rs) (g_free rects))
             rs))
  #:c-id poppler_page_get_text_layout)

(define (page-text-with-layout maybe-page)
  (define page (to-page maybe-page))
  ;; ^^ this is unbearably slow if we have to reopen the document for
  ;; each letter
  (for/list ([box (page-text-layout page)])
    (define text (apply page-text-in-rect page 'glyph box))
    (list text box)))

(define-poppler page-render-to-cairo!
  (_fun (maybe-page cairo-context) ::
        [page-ptr : _PopplerPagePointer = (to-page maybe-page)]
        [cairo-context : _cairo_t]
        -> _void)
  #:c-id poppler_page_render)

(define (page-render-to-dc! maybe-page dc)
  ;; Render the given page of the PDF file to the given dc.
  (define tr (send dc get-transformation))
  (send dc in-cairo-context
        (λ(cairo_ctx)
          (page-render-to-cairo! maybe-page cairo_ctx)))
  (send dc set-transformation tr))

(define (page->bitmap maybe-page)
  ;; Render the given page of the PDF file to a new bitmap.
  (define page (to-page maybe-page))
  (match-define (list width height) (page-size page))
  (define bm (make-object bitmap%
                          (inexact->exact (ceiling width))
                          (inexact->exact (ceiling height))
                          #f #t))
  (page-render-to-dc! page (new bitmap-dc% [bitmap bm]))
  bm)

(define (page->pict maybe-page)
  (define pg (to-page maybe-page))
  (match-define (list width height) (page-size pg))
  (dc (λ(ctx x y)
        (define tr (send ctx get-transformation))
        (send ctx translate x y)
        (page-render-to-dc! pg ctx)
        (send ctx set-transformation tr))
      width
      height))

(define-poppler pdf-title
  (_fun (maybe-doc) ::
        [doc-ptr : _PopplerDocumentPointer = (to-doc maybe-doc)]
        -> _string)
  #:wrap (allocator g_free)
  #:c-id poppler_document_get_title)

(define-poppler pdf-author
  (_fun (maybe-doc) ::
        [doc-ptr : _PopplerDocumentPointer = (to-doc maybe-doc)]
        -> _string)
  #:wrap (allocator g_free)
  #:c-id poppler_document_get_author)

(define-poppler pdf-subject
  (_fun (maybe-doc) ::
        [doc-ptr : _PopplerDocumentPointer = (to-doc maybe-doc)]
        -> _string)
  #:wrap (allocator g_free)
  #:c-id poppler_document_get_subject)

(define-poppler pdf-keywords
  (_fun (maybe-doc) ::
        [doc-ptr : _PopplerDocumentPointer = (to-doc maybe-doc)]
        -> _string)
  #:wrap (allocator g_free)
  #:c-id poppler_document_get_keywords)

(define-poppler pdf-creator
  (_fun (maybe-doc) ::
        [doc-ptr : _PopplerDocumentPointer = (to-doc maybe-doc)]
        -> _string)
  #:wrap (allocator g_free)
  #:c-id poppler_document_get_creator)

(define-poppler pdf-producer
  (_fun (maybe-doc) ::
        [doc-ptr : _PopplerDocumentPointer = (to-doc maybe-doc)]
        -> _string)
  #:wrap (allocator g_free)
  #:c-id poppler_document_get_producer)

(define-poppler page-label
  (_fun (maybe-page) ::
        [doc-ptr : _PopplerPagePointer = (to-page maybe-page)]
        -> _string)
  #:wrap (allocator g_free)
  #:c-id poppler_page_get_label)