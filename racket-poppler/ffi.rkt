#lang racket
(require "libs.rkt"
         ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define
         ffi/unsafe/atomic
         racket/draw
         racket/draw/unsafe/cairo
         racket/draw/private/local ; needed for in-cairo-context
         racket/draw/unsafe/pango  ; needed for g_object_unref
         racket/draw/unsafe/glib
         (only-in slideshow/pict dc)
         (only-in web-server/private/util bytes-ci=?))

(provide (all-defined-out))

(define-runtime-lib poppler-lib
  [(unix) 
   (ffi-lib "libpoppler-glib" '("8" ""))]
  [(macosx)
   ; order these so dependencies are loaded first
   (ffi-lib "libz.1.dylib")
   ; (ffi-lib "libintl.8.dylib")
   (ffi-lib "libintl.9.dylib")
   (ffi-lib "libpng16.16.dylib")
   (ffi-lib "libexpat.1.dylib")
   (ffi-lib "libfreetype.6.dylib")
   (ffi-lib "libfontconfig.1.dylib")
   (ffi-lib "libpixman-1.0.dylib")
   (ffi-lib "libcairo.2.dylib")
   (ffi-lib "libjpeg.9.dylib")
   (ffi-lib "libpoppler.44.dylib")
   (ffi-lib "libgio-2.0.0.dylib")
   (ffi-lib "libpoppler-glib.8.dylib")]
  [(windows)
   ; order these so dependencies are loaded first
   (ffi-lib "zlib1.dll")
   (ffi-lib "libintl-9.dll")
   (ffi-lib "libpng16-16.dll")
   (ffi-lib "libexpat-1.dll")
   (ffi-lib "libfreetype-6.dll")
   (ffi-lib "libfontconfig-1.dll")
   (ffi-lib "libpixman-1-0.dll")
   (ffi-lib "libcairo-2.dll")
   (ffi-lib "libpoppler-44.dll")
   (ffi-lib "libgio-2.0-0.dll")
   (ffi-lib "libpoppler-glib-8.dll")])

(define-ffi-definer define-poppler poppler-lib)


;;;
;;; GLib Double Linked Lists
;;;

;; Docs: https://developer.gnome.org/glib/2.30/glib-Doubly-Linked-Lists.html



(define-cstruct _GList  ; double linked list links,
  ([data _pointer]      ; pointing to data
   [next _GList-pointer/null]
   [prev _GList-pointer/null]))

(define (gdata g*) (and g* (GList-data (ptr-ref g* _GList))))
(define (gnext g*) (and g* (GList-next (ptr-ref g* _GList))))
(define (gprev g*) (and g* (GList-prev (ptr-ref g* _GList))))

(define _GList* _GList-pointer)
(define _GList*/null _GList-pointer/null)

; In C the empty _GList is represented as null, in Racket as #f.
; In other words, we must use _GList*/null  for functions that
; return pointers to GLists that might be null.

(define (gempty? g*) (not g*)) ; is the glist empty?
; gfirst returns the first element (or #f for an empty glist)
(define-glib g_list_first (_fun _GList* -> _GList*/null))
(define (gfirst g*) (and g* (g_list_first g*)))
; g_list_free frees the links of the list (the datas are not freed)
(define-glib g_list_free (_fun _GList* -> _void))
(define (glistfree g*) (and g* (g_list_free g*)))
; g_list_free frees both the links of the list and the datas
(define-glib g_list_free_full (_fun _GList* -> _void))
(define (glistfreefull g*) (and g* (g_list_free_full g*)))

; glist->list : GList*/null -> list
;   return a list with the elements of the glist,
(define (glist->list g*)
  (cond [(gempty? g*) '()]
        [else (cons (gdata g*)
                    (glist->list (gnext g*)))]))

; g_list_length returns the length (takes time O(n))
(define-glib g_list_length (_fun _GList* -> _uint))
(define (glength g) (if g (g_list_length g) 0))
; g_list_nth_data returns a pointer to the nth element
(define-glib g_list_nth_data (_fun _GList* _uint -> _pointer))

(define-glib g_filename_to_uri (_fun _string _string #f -> _string))

;;;
;;; Poppler Documents
;;;

; Poppler represents Documents as PopplerDocument cstructs.
(define-cpointer-type _PopplerDocumentPointer)

; In Racket we will represent them as a pdf structure.
(define-struct pdf (pointer))

; Poppler provides poppler_document_new_from_file to open
; a pdf document, given a uri.

(define-poppler unsafe-pdf-from-file
  (_fun [uri : _string]
        [password : _string]
        [err : _pointer = #f]
        -> [return : (_or-null _PopplerDocumentPointer)])
  #:c-id poppler_document_new_from_file
  #:wrap (allocator g_object_unref))

; pdf-file? (or path string) -> boolean
(define (pdf-file? path)  
  (and (file-exists? path)
       (bytes=? (filename-extension path) #"pdf")))

; open-pdf : path-or-string [(or string #f)] -> (or document #f)
;   if p is the (relative or full) path of a pdf-file, open it
(define (open-pdf p [password #f])
  (and (pdf-file? p)
       (let ([p (cond [(complete-path? p) p]
                      [(relative-path? p) (path->complete-path p)]
                      [(string? p)        (open-pdf (string->path p) password)]
                      [else               (error 'open-pdf "internal error")])])
         (define pointer (unsafe-pdf-from-file (g_filename_to_uri (path->string p) #f #f) password))
         (cond [pointer => pdf] [else #f]))))

; pdf-count-pages : document -> integer
;   return the number of pages in the document
(define-poppler pdf-count-pages
  (_fun (doc) :: 
        [doc-ptr : _PopplerDocumentPointer = (pdf-pointer doc)]
        -> _int)
  #:c-id poppler_document_get_n_pages)

(define-syntax (define-info-getter stx)
  (syntax-case stx ()
    [(_ id ffi-id)
     #'(define-poppler id
         (_fun (doc) ::  [doc-ptr : _PopplerDocumentPointer = (pdf-pointer doc)] -> _string)
         #:c-id ffi-id)]))

(define-info-getter pdf-title    poppler_document_get_title)
(define-info-getter pdf-author   poppler_document_get_author)
(define-info-getter pdf-subject  poppler_document_get_subject)
(define-info-getter pdf-keywords poppler_document_get_keywords)
(define-info-getter pdf-creator  poppler_document_get_creator)
(define-info-getter pdf-producer poppler_document_get_producer)

;;;
;;; Poppler Pages
;;;

; Poppler represents a page as a PopplerPages cstruct.
(define-cpointer-type _PopplerPagePointer)
; In Racket we represent them as a page struct, which besides
; the pointer to the cstruct also references the document.
; This way the garbage collector will not free the document
; until after all pages have been freed.
(define-struct page (document pointer))

(define-poppler unsafe-get-page
  (_fun (doc index) ::
        [doc-ptr : _PopplerDocumentPointer = (pdf-pointer doc)]
        [index : _int]
        -> _PopplerPagePointer)
  #:c-id poppler_document_get_page
  #:wrap (allocator g_object_unref))

; pdf-page : pdf index -> (or page #f)
;   open page with index i of the document d,
(define (pdf-page d i)
  (and (< i (pdf-count-pages d))
       (page d (unsafe-get-page d i))))

; page-size : page -> (list number number)
;   return list of width and height of the page,
;   the unit of measurement is points
(define-poppler page-size 
  (_fun (p) ::
        [page-ptr : _PopplerPagePointer = (page-pointer p)]
        [width  : (_ptr o _double)]
        [height : (_ptr o _double)]
        -> _void
        -> (list width height))
  #:c-id poppler_page_get_size)

(define (page-height p) (second (page-size p)))
(define (page-width p)  (first  (page-size p)))

; page-crop-box : page -> rectangle?
(define-poppler page-crop-box ; 
  (_fun (p) ::
        [page-ptr : _PopplerPagePointer = (page-pointer p)]
        [r : (_ptr o _PopplerRectangle)]
        -> _void
        -> (rectangle->list r (page-height p)))
  #:c-id poppler_page_get_crop_box)

; A selection style determines the minimum unit of selection
(define _SelectionStyles (_enum '(glyph word line)))

; page-text-in-rect : page style R R R R -> string?
;   TODO: convert back and forth between coordinate systems
(define-poppler page-text-in-rect
  (_fun (p style x1 y1 x2 y2) ::
        [page-ptr : _PopplerPagePointer    = (page-pointer p)]
        [style : _SelectionStyles]
        [rect : (_ptr i _PopplerRectangle) = (make-PopplerRectangle x1 y1 x2 y2)]
        -> _string)
  ; #:wrap (allocator g_free)
  #:c-id poppler_page_get_selected_text)

; page-text : page -> string
;   return all text on page
(define-poppler page-text
  (_fun (p) :: [page-ptr : _PopplerPagePointer = (page-pointer p)]
        -> _string)
  ; #:wrap (allocator g_free)
  #:c-id poppler_page_get_text)

; page-find-text : page string -> (listof retangle?)
;   findes text on the page (using default options),
;   the result is a list of rectangles for each occurance of the text
#;(define-poppler page-find-text
  (_fun (p text) ::
        [page-ptr : _PopplerPagePointer = (page-pointer p)]
        [text : _string]         ; string utf8 encoded
        -> [grs : _GList*/null]  ; A GList of PopplerRectangle
        ; poppler returns "PDF coordinates", 
        ; giving grectangles->list a height flips them.
        -> (glist-of-rectangles->list-of-lists grs (page-height p)))
  #:wrap (allocator glistfreefull)
  #:c-id poppler_page_find_text)


(define-poppler raw-page-find-text
  (_fun (p text) ::
        [page-ptr : _PopplerPagePointer = (page-pointer p)]
        [text : _string]         ; string utf8 encoded
        -> [grs : _GList*/null])  ; A GList of PopplerRectangle
        ; poppler returns "PDF coordinates", 
        ; giving grectangles->list a height flips them.
  #:wrap (allocator glistfreefull)
  #:c-id poppler_page_find_text)

(define (page-find-text p text)
  (define grs (raw-page-find-text p text))
  (glist-of-rectangles->list-of-lists grs (page-height p)))

  
#;(define-poppler page-find-text
  (_fun (p text) ::
        [page-ptr : _PopplerPagePointer = (page-pointer p)]
        [text : _string]         ; string utf8 encoded
        -> [grs : _GList*/null]  ; A GList of PopplerRectangle
        ; poppler returns "PDF coordinates", 
        ; giving grectangles->list a height flips them.
        -> #;(glist-of-rectangles->list-of-lists grs (page-height p))
        (begin0 
             (glist-of-rectangles->list-of-lists grs (page-height p))
             (glistfreefull grs)))
  ; #:wrap (allocator glistfreefull)
  #:c-id poppler_page_find_text)


; page-text-layout : page -> (listof rectangles?)
;   Return a list of rectangles. Each rectangle
;   corresponds to a character of (page-text p)
(define-poppler page-text-layout
  (_fun (p) :: [page-ptr : _PopplerPagePointer = (page-pointer p)]
        ; output:
        [rs : (_ptr o _pointer)] ; PopplerRectangle  **rs
        [n  : (_ptr o _uint)]    ; guint n                 (number of rectangles)
        -> [rglist : _bool]      ; true if page contains text, false otherwise
        -> (let () (define prs (map PopplerRectangle->list 
                                    (cblock->list rs _PopplerRectangle n)))
             ; only call g_free, if there are rectangles to free
             (unless (empty? prs) (g_free rs))
             prs))
  #:c-id poppler_page_get_text_layout)

; page-text-with-layout : page -> (listof string? rectangle?)
(define (page-text-with-layout p)
  ; TODO: why not combine page-text and page-text-layout? 
  (define page (page-pointer p)) ; open page once only
  (for/list ([box (page-text-layout page)])
    (define text (apply page-text-in-rect page 'glyph box))
    (list text box)))


; page-label : page -> string
(define-poppler page-label
  (_fun (p) :: [ptr : _PopplerPagePointer = (page-pointer p)] -> _string)
  #:wrap (allocator g_free) #:c-id poppler_page_get_label)

;;;
;;; Poppler Page Layout
;;;

; PopplerPageLayout


;;;
;;; Poppler Rectangles
;;;

; Poppler represents rectangles as PopplerRectangle cstructs.
(define-cstruct _PopplerRectangle  ([x1 _double] [y1 _double] [x2 _double] [y2 _double]))
; (x1,y1) is lower left corner. (x2,y2) is upper right corner
; The coordinate system is a standard xy-coordinate system. The units are points.

; In Racket the rectangle will be represented as a list: (list x1 y1 x2 y2).
(define rectangle?
  (list/c (and/c real? (not/c negative?)) (and/c real? (not/c negative?))
          (and/c real? (not/c negative?)) (and/c real? (not/c negative?))))

; The Racket coordinates use a flipped y-axis (top of screen = 0).
;   racket_y = pageheight - poppler_y

; rectangle->list : poppler-rectangle number -> rectangle?
;  Convert the PopplerRectangle into (list x1 y1 x2 y2).
;  If height is non-zero, the y-coordinates are flipped,
;  and the coordinates can be used directly with Racket picts.
(define (rectangle->list r [height #f])
  (define l (PopplerRectangle->list r))
  (cond [height (match-define (list x1 y1 x2 y2) l)
                (define (flip y) (- height y))
                (list x1 (flip y2) x2 (flip y1))]
        [else l]))

;;;
;;; GLists of Poppler Rectangles
;;; 

; Functions such as poppler_page_get_text_layout return a GList of PopplerRectangles.
; We need a conversion to Racket lists of Racket rectangles.

; grectangles->list/free : GList*/null -> (list (list number number number number))
;   Converts a GList of PopperRectangles to a (Racket) list.
;   Both the GList and the rectangles are freed.
(define (glist-of-rectangles->list-of-lists grs [height #f])
  (for/list ([r (in-list (glist->list grs))])
    (rectangle->list (cast r _pointer _PopplerRectangle-pointer) height)))

;;;
;;; Rendering
;;;

(define-poppler page-render-to-cairo!
  (_fun (p cairo-context) ::
        [page-ptr : _PopplerPagePointer = (page-pointer p)]
        [cairo-context : _cairo_t]
        -> _void)
  #:c-id poppler_page_render)

(define-poppler page-render-for-printing-to-cairo!
  (_fun (p cairo-context) ::
        [page-ptr : _PopplerPagePointer = (page-pointer p)]
        [cairo-context : _cairo_t]
        -> _void)
  #:c-id poppler_page_render_for_printing)

(define _PopplerPrintFlags
  (_enum '(POPPLER_PRINT_DOCUMENT          = 0
                                           POPPLER_PRINT_MARKUP_ANNOTS     = 1
                                           POPPLER_PRINT_STAMP_ANNOTS_ONLY = 2
                                           POPPLER_PRINT_ALL               = 1)))

(define-poppler page-render-for-printing-with-options-to-cairo!
  (_fun (p cairo-context options) ::
        [page-ptr : _PopplerPagePointer = (page-pointer p)]
        [cairo-context : _cairo_t]
        [options : _PopplerPrintFlags]
        -> _void)
  #:c-id poppler_page_render_for_printing_with_options)

(define (page-render-to-dc! p dc [options 'POPPLER_PRINT_DOCUMENT])
  ;; Render the given page of the PDF file to the given dc.
  (define tr (send dc get-transformation))
  (send dc in-cairo-context
        (λ (cairo_ctx) (page-render-for-printing-with-options-to-cairo! p cairo_ctx options)))
  (send dc set-transformation tr))

(define (page->bitmap p)
  ;; Render the given page of the PDF file to a new bitmap.
  (match-define (list width height) (page-size p))
  (define bm (make-object bitmap%
               (inexact->exact (ceiling width))
               (inexact->exact (ceiling height))
               #f #t))
  (page-render-to-dc! p (new bitmap-dc% [bitmap bm]))
  bm)

(define-syntax-rule (with-cr default cr . body)
  ;; Faster:
  (begin
    (start-atomic)
    (let ([cr cr])  ; [cr (get-cr)]
      (if cr 
          (begin0
            (begin . body)
            ; (release-cr cr) ; corresponded to (get-cr)
            (end-atomic))
          (begin
            (end-atomic)
            default))))
  ;; Safer:
  #;
  (call-as-atomic
   (lambda ()
     (let ([cr cr])
       (if cr 
           (dynamic-wind
            void
            (lambda () . body) 
            (lambda () (release-cr cr)))
           default)))))

(define (page->pict p [α 1.0] [options 'POPPLER_PRINT_DOCUMENT])
  (match-define (list w h) (page-size p)) ; in points
  ; (define α 1.0) ; TODO: What is the correct factor here? TODO : it seems this factor is ignored?!?
  (define-values (width height) (values (* α w) (* α h))) ; convert from point to pixels
  (dc 
   (λ(dc x y)
     (define tr (send dc get-transformation))
     (send dc translate x y)
     (when (or (is-a? dc bitmap-dc%)
               (is-a? dc svg-dc%)
               (is-a? dc pdf-dc%)
               (is-a? dc post-script-dc%))
       ; (define bm (send dc get-bitmap))
       (send dc in-cairo-context
             (λ (target-cr)
               (cairo_save target-cr)
               (cairo_scale target-cr α α)
               (page-render-for-printing-with-options-to-cairo! p target-cr options)
               (cairo_restore target-cr))))     
     (send dc set-transformation tr))
   width height))

#;(define (page->pict p [options 'POPPLER_PRINT_DOCUMENT])
    #;(define use-recording? #f) ; flag for debug
    (match-define (list w h) (page-size p)) ; in points
    (define α 1.0) ; TODO: What is the correct factor here? TODO : it seems this factor is ignored?!?
    (define-values (width height) (values (* α w) (* α h))) ; convert from point to pixels
    #;(define (make-recording) ; -> surface
        (define r (make-cairo_rectangle_t 0.0 0.0 width height))
        (define recorded-surface (cairo_recording_surface_create CAIRO_CONTENT_COLOR_ALPHA r)) ; unbounded
        (define recorded-cairo (cairo_create recorded-surface))
        (cairo_save recorded-cairo)
        (cairo_scale recorded-cairo α α)
        (page-render-for-printing-with-options-to-cairo! p recorded-cairo options)
        (cairo_restore recorded-cairo)
        recorded-surface)
    #;(define recorded-surface (and use-recording? (make-recording)))
    (dc 
     (λ(dc x y)
       #;(displayln (~a "redrawing at " x "," y))
       (define tr (send dc get-transformation))
       (send dc translate x y)
       #;(displayln (list 'bitmap-dc? (is-a? dc bitmap-dc%) width height dc))
       (when (is-a? dc bitmap-dc%)
         (define bm (send dc get-bitmap))
         (send dc in-cairo-context
               (λ (target-cr)
                 (cairo_save target-cr)
                 (cairo_scale target-cr α α)
                 (cond 
                   #;[use-recording?
                      (cairo_set_operator target-cr CAIRO_OPERATOR_DEST_OVER)
                      (cairo_set_source_surface target-cr recorded-surface 0 0)
                      (cairo_paint target-cr)]
                   [else
                    (page-render-for-printing-with-options-to-cairo! p target-cr options)])
                 (cairo_restore target-cr))))
       #;(when (is-a? dc record-dc%)
           ; a record-dc% always uses a recorded surface
           (unless recorded-surface
             (set! recorded-surface (make-recording)))
           (send dc record-cairo
                 (λ (target-cr)
                   (displayln "pict receiving cr for surface to draw on")
                   (cairo_save target-cr)
                   (cairo_set_operator target-cr CAIRO_OPERATOR_DEST_OVER)
                   (cairo_set_source_surface target-cr recorded-surface 0 0)
                   (cairo_paint target-cr)
                   (cairo_restore target-cr)
                   
                   ;(cairo_set_line_width target-cr 1.0)
                   ;(cairo_set_source_rgb target-cr 1.0 0.0 0.5)
                   ;(cairo_move_to target-cr 0.0 0.0)
                   ;(cairo_line_to target-cr 10.0 10.0)
                   ; (cairo_stroke target-cr)
                   ; (page-render-for-printing-with-options-to-cairo! p target-cr options)
                   ; (cairo_restore target-cr)
                   )))
       (send dc set-transformation tr))
     width height))
