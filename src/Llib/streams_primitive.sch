;;;; Copyright (C) Joseph Donaldson (2014). All Rights Reserved.
;;;; Adapted for Bigloo

;;;; Copyright (C) Philip L. Bewig (2007). All Rights Reserved.

;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:

;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;;; SOFTWARE.

(define-syntax stream-lazy
   (syntax-rules ()
      ((stream-lazy expr)
       (make-stream
          (cons 'lazy (lambda () expr))))))

(define-syntax stream-delay
   (syntax-rules ()
      ((stream-delay expr)
       (stream-lazy (stream-eager expr)))))

(define-syntax stream-cons
   (syntax-rules ()
      ((stream-cons obj strm)
       (stream-eager (make-stream-pare (stream-delay obj) (stream-lazy strm))))))


(define-syntax stream-lambda
   (syntax-rules ()
      ((stream-lambda formals body0 body1 ...)
       (lambda formals (stream-lazy (let () body0 body1 ...))))))