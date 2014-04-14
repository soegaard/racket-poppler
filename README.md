racket-poppler
==============

This package allows you to read pdf files, render them to
a drawing context and more.

As an application this package includes render-tex, which
allows you to convert snippets of TeX into scalable picts.
This can be used to make pretty mathematical formuals
in slideshow and scribble.

The underlying work horse of this package is Poppler.
Poppler is a C libary for rendering pdfs via Cairo.
The license of Poppler is GPL.

In order for the rendering to work the Poppler library needs
to be compiled to use the same version of Cairo as Racket uses.
For that reason racket-poppler is distributed with Poppler 
precompiled for Windows and OS X. Thanks to Matthew Flatt for 
providing the binaries. The license of Poppler is GPL,
so it can't be distributed with Racket.

Note:  Use the development version of Racket - which uses a newer
       version of Cairo than the current release.

The racket-poppler package is based on code by Michael Wilber 
(https://github.com/gcr/pdf-render).

Example (assumes latex is installed):

    > (require racket-poppler/render-tex)
    > (latex-path "/usr/local/texlive/2013/bin/universal-darwin/pdflatex")
    > (define p (latex->pict "$\\sqrt{x^2+y^2}$"))
    > (pict->bitmap p)

![nice picture in DrRacket](http://i.imgur.com/HiHFAQ1.png)

Now let's do something with the picture:

    > (define beside hc-append)
    > (define above  vc-append)

    > (pict->bitmap (above (beside (rotate p (- pi (/ pi 3))) (rotate p    (/ pi 3)))
                           (beside (rotate p (+ pi (/ pi 3))) (rotate p (- (/ pi 3))))))

![nice picture in DrRacket](http://i.imgur.com/LT9j8cl.png)

This renders the formula of Pythagoras as a pdf, converts it into 
a pict, which is then rotated. The rotated pict is converted into
a bitmap, and finally DrRacket shows the bitmap.

Note: DrRacket will currently not display the picts generated 
by latex->pict or pdf->pict directly. Wrap the picts in
pict->bitmap to see them in DrRacket. (The cause of this
is fact that pdf->pict draws directly the Cairo drawing context.
DrRacket "copies" picts using a record-dc% and recording 
contexts doesn't record drawing operations that draw directly
to the Cairo context.)

See Michael Wilber's original documentation for some ideas 
on how to use this library, but note that the API has changed
slightly. In racket-poppler all operations are safe.
http://planet.racket-lang.org/package-source/gcr/pdf-render.plt/2/2/planet-docs/main/index.html

For an extensive example see "examples/test-pdf-functions.rkt".

Warning: The documentation in doc/ has not been updated yet.

[1] OS X binaries for libpoppler and friends
    https://github.com/soegaard/racket-osx-libs/tree/master/lib

/soegaard


