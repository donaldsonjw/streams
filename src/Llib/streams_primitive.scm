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

(module streams-primitive
   (include "streams_primitive.sch")
   (export (final-class stream
	      promize::pair)
	   (abstract-class stream-base)
	   (final-class stream-nil::stream-base)
	   (final-class stream-pare::stream-base
	      (kar read-only)
	      (kdr read-only))
	   stream-null
	   (inline make-stream-pare kar kdr)
	   (inline make-stream prom)
	   (stream-eager expr)
	   (stream-pair? obj)
	   (stream-null? obj)
	   (stream-car strm::stream)
	   (stream-cdr strm)
	   
	   (inline stream? strm)))
   #;(export stream-null stream-cons stream? stream-null? stream-pair?
          stream-car stream-cdr stream-lambda)


(define-inline (make-stream prom)
   (instantiate::stream (promize prom)))

(define-inline (stream? strm)
   (isa? strm stream))


(define (stream-eager expr)
   (make-stream
      (cons 'eager expr)))


(define (stream-force promise::stream)
   (let ((content::pair (-> promise promize)))
      (case (car content)
	 ((eager) (cdr content))
	 ((lazy)  (let* ((promise*::stream ((cdr content)))
			 (content  (-> promise promize)))
		     (if (not (eqv? (car content) 'eager))
			 (begin (set-car! content (car (-> promise* promize)))
				(set-cdr! content (cdr (-> promise* promize)))
				(set! (-> promise* promize) content)))
		     (stream-force promise))))))

(define stream-null (stream-delay (cons 'stream 'null)))

(define-inline (make-stream-pare kar kdr)
   (instantiate::stream-pare (kar kar) (kdr kdr)))

(define-inline (stream-pare? strm-pare)
   (isa? strm-pare stream-pare))

(define (stream-pair? obj)
   (and (stream? obj) (stream-pare? (stream-force obj))))

(define (stream-null? obj)
   (and (stream? obj)
	(eqv? (stream-force obj)
	   (stream-force stream-null))))




(define-inline (stream-kar pair::stream-pare)
   (-> pair kar))

(define-inline (stream-kdr pair::stream-pare)
   (-> pair kdr))


(define (stream-car strm::stream)
   (cond ((not (stream? strm)) (error "stream-car" "non-stream" strm))
	 ((stream-null? strm) (error "stream-car" "null stream" strm))
	 (else (stream-force (stream-kar (stream-force strm))))))

(define (stream-cdr strm)
   (cond ((not (stream? strm)) (error 'stream-cdr "non-stream" stream))
	 ((stream-null? strm) (error 'stream-cdr "null stream" stream))
	 (else (stream-kdr (stream-force strm)))))

