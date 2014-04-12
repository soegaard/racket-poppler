#lang racket
(require racket-poppler pict)

; document-info : d -> assoc-list
(define (document-info d)
  (list (list 'title    (pdf-title d))
        (list 'author   (pdf-author d))
        (list 'aubject  (pdf-subject d))
        (list 'keywords (pdf-keywords d))
        (list 'creator  (pdf-creator d))
        (list 'producer (pdf-producer d))
        (list 'page-count (pdf-count-pages d))))

; (define f "x.pdf")
(define f "guide.pdf")
(pdf-file? f)
(define d (open-pdf f))
d
(pdf-title d)
(document-info d)
(define p (pdf-page d 0))
p
(page-size p)
(page-crop-box p)
(page-text-in-rect p 'word 0. 0. 500. 500.)
(page-text p)
; (page-find-text p "the")
(define (take<= xs n) (for/list ([x xs][i n]) x))
(take<= (page-find-text p "not on the page") 5)
(take<= (page-text-layout p) 5)

(page->bitmap p)

;(pict->bitmap (scale (page->pict p) 0.75))

(list
 (pict->bitmap (scale (page->pict p) 0.75))
 (pict->bitmap
  (scale 
   (for/fold ([pageview (apply blank (page-size p))])
     ([box (in-list (page-text-layout p))])
     (match-define (list x1 y1 x2 y2) box)
     (pin-over pageview x1 y1
               (colorize (rectangle (- x2 x1) (- y2 y1)) "gray")))
   0.75)))

;(page-find-text p "the")

;; Overlay each box over the PDF.
(pict->bitmap
 (for/fold ([pageview (page->pict p)])
  ([bounding-box (in-list (page-find-text p "the"))])
  (match-define (list x1 y1 x2 y2) bounding-box)
  ;; Each match's bounding box ^
  (pin-under pageview x1 y1
            (cellophane
             (colorize (filled-rectangle (- x2 x1) (- y2 y1)) "yellow")
             0.5))))

;(define the-layout (page-text-layout p))
;(define the-text (page-text p))
;(length the-layout)
;(string-length the-text)
