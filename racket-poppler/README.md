racket-poppler
==============

Poppler is a library for reading and rendering pdf-files.
The racket-poppler package contains bindings for poppler.

This package allows you to read pdf files, render them to
a drawing context and more.

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

Example:

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
by latex->pict or pdf->pict. The reason is that pdf->pict draws
directly to a Cairo drawing context.

See Michael Wilber's original documentation for some ideas.
http://planet.racket-lang.org/package-source/gcr/pdf-render.plt/2/2/planet-docs/main/index.html
Note: The interface has changed. Now all functions are safe. See "main.rkt".

For an extensive example see "examples/pdf-to-txt.rkt".
Despite the name the examples show of quite a few functions.

Warning: The documentation in doc/ has not been updated yet.

[1] OS X binaries for libpoppler and friends
    https://github.com/soegaard/racket-osx-libs/tree/master/lib

/soegaard


