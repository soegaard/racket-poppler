racket-poppler
==============

Racket bindings for Poppler (library for reading and generating pdfs)

The library was tested on OS X using a version of libpoppler compiled
with the same version of Cairo as DrRacket uses. Do *not* expect other
versions of libpoppler to work. See [1] for binaries.
for binaries.

The library is based on code by Michael Wilber (https://github.com/gcr/pdf-render).

Example:
    > (require racket-poppler/render-tex)
    > (pict->bitmap (rotate (latex->pict "$\\sqrt{c^2=a^2+b^2}$") 3.1415))
    <a rotated formula is shown in DrRacket>

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


