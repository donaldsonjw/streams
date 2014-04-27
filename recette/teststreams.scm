;;;; Copyright (C) Joseph Donaldson (2014). All Rights Reserved.
;;;; Bigloo Adaption of srfi-41

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

(module teststreams
   (library btest streams)
   (main main))


(define fibs
   (stream-cons 1
      (stream-cons 1
	 (stream-map +
	    fibs
	    (stream-cdr fibs)))))

(define nats (stream-cons 0 (stream-map (lambda (x) (+ x 1)) nats)))

(define (lsec proc . args)
   (lambda x (apply proc (append args x))))

(define hamming
   (stream-cons 1 
      (stream-unique =
	 (stream-merge <
	    (stream-map (lsec * 2) hamming)
	    (stream-map (lsec * 3) hamming)
	    (stream-map (lsec * 5) hamming)))))


(define (len strm)
   (stream-match strm
      (() 0)
      ((head . tail) (+ 1 (len tail)))))
   

(define-test-suite streams-suite

   (test "cond-expand streams"
      (assert-true (cond-expand (streams #t)
				(else #f))))

   (test "cond-expand srfi-41"
      (assert-true (cond-expand (srfi-41 #t)
				(else #f))))

   (test "list->stream '(1 2 3)"
      (assert-equal? (stream->list (list->stream '(1 2 3)))
	 '(1 2 3)))
   
   (test "stream->list (stream 1 2 3)"
      (assert-equal? (stream->list (stream 1 2 3))
	 '(1 2 3)))

   (test "port->stream"
      (let* ((in (open-input-string "abc"))
	     (str (port->stream in)))
	 (assert-equal? (stream->list str)
	    '(#\a #\b #\c))))
	 
   (test "stream-append (stream 1 2 3) (stream 4 5 6)"
      (assert-equal? (stream->list (stream-append (stream 1 2 3)
				      (stream 4 5 6)))
	 '(1 2 3 4 5 6)))

   (test "stream-concat (stream (stream 1 2 3) (stream 4 5 6))"
      (assert-equal? (stream->list (stream-concat (stream  (stream 1 2 3)
						     (stream 4 5 6))))
	 '(1 2 3 4 5 6)))

   (test "stream-constant 3"
      (assert-equal? (stream->list (stream-take 3 (stream-constant 3)))
	 '(3 3 3)))

   (test "stream-drop 4 nats"
      (assert-equal? (stream-ref (stream-drop 4 nats) 0)
	 4))

   (test "stream-drop-while < 4 nats"
      (assert-equal? (stream-ref (stream-drop-while (lambda (x) (< x 4)) nats)
			0)
	 4))

   (test "stream-filter odd? (stream 1 2 3 4 5 6)"
      (assert-equal? (stream->list (stream-filter odd? (stream 1 2 3 4 5 6)))
	 '(1 3 5)))
   
   (test "first 4 of the fibonacci sequence"
      (assert-equal? (stream->list (stream-take 4 fibs))
	    '(1 1 2 3)))

   (test "stream-match: match empty stream"
      (assert-true (stream-match (stream)
		       (() #t)
		       (else #f))))
   
   (test "stream-match: does not match empty stream"
      (assert-false (stream-match (stream 1 2 3)
		      (() #t)
		       (else #f))))

   (test "stream-match: match head"
      (assert-equal? (stream-match (stream 1 2 3)
		      ((d . rest) d)
		       (else #f))
	 1))

   
   (test "stream-match: match tail"
      (assert-equal? (stream-match (stream 1 2 3)
			((d . rest) (stream->list rest))
			(else #f))
	 '(2 3)))

   

   )
   

(define (main args)
   (let ((tr (instantiate::terminal-test-runner (suite streams-suite))))
	 (test-runner-execute tr #t)))

