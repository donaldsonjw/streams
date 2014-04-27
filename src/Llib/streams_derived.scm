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

(module streams-derived
   (import streams-primitive)
   (include "streams_primitive.sch"
	    "streams_derived.sch")
   (export (list->stream objs)
	   (port->stream . port)
	   (stream->list . args)
	   (stream-append . strms)
	   (stream-concat strms)
	   stream-constant
	   (stream-drop n strm)
	   (stream-drop-while pred? strm)
	   (stream-filter pred? strm)
	   (stream-fold proc base strm)
	   (stream-for-each proc . strms)
	   (stream-from first . step)
	   (stream-iterate proc base)
	   (stream-length strm)
	   (stream-map proc . strms)
	   (stream-range first past . step)
	   (stream-ref strm n)
	   (stream-reverse strm)
	   (stream-scan proc base strm)
	   (stream-take n strm)
	   (stream-take-while pred? strm)
	   (stream-unfold mapper pred? generator base)
	   (stream-unfolds gen seed)
	   (stream-zip . strms)
	   stream-merge
	   stream-unique))

;;;; exists is a synonym for find
(define exists find)


(define (list->stream objs)
   (define list->stream
      (stream-lambda (objs)
	 (if (null? objs)
	     stream-null
	     (stream-cons (car objs) (list->stream (cdr objs))))))
   (if (not (list? objs))
       (error 'list->stream "non-list argument" objs)
       (list->stream objs)))

(define (port->stream . port)
   (define port->stream
      (stream-lambda (p)
	 (let ((c (read-char p)))
	    (if (eof-object? c)
		stream-null
		(stream-cons c (port->stream p))))))
   (let ((p (if (null? port) (current-input-port) (car port))))
      (if (not (input-port? p))
          (error 'port->stream "non-input-port argument" p)
          (port->stream p))))


(define (stream->list . args)
   (let ((n (if (= 1 (length args)) #f (car args)))
	 (strm (if (= 1 (length args)) (car args) (cadr args)))) 
      (cond ((not (stream? strm)) (error 'stream->list "non-stream argument" strm))
            ((and n (not (integer? n))) (error 'stream->list "non-integer count" n))
            ((and n (negative? n)) (error 'stream->list "negative count" n))
            (else (let loop ((n (if n n -1)) (strm strm))
		     (if (or (zero? n) (stream-null? strm))
			 '()
			 (cons (stream-car strm) (loop (- n 1) (stream-cdr strm)))))))))

(define (stream-append . strms)
   (define stream-append
      (stream-lambda (strms)
	 (cond ((null? (cdr strms)) (car strms))
	       ((stream-null? (car strms)) (stream-append (cdr strms)))
	       (else (stream-cons (stream-car (car strms))
			(stream-append (cons (stream-cdr (car strms)) (cdr strms))))))))
    (cond ((null? strms) stream-null)
          ((exists (lambda (x) (not (stream? x))) strms)
	   (error 'stream-append "non-stream argument" strms))
          (else (stream-append strms))))

(define (stream-concat strms)
    (define stream-concat
       (stream-lambda (strms)
	  (cond ((stream-null? strms) stream-null)
		((not (stream? (stream-car strms)))
		 (error 'stream-concat "non-stream object in input stream"strms))
		((stream-null? (stream-car strms))
		 (stream-concat (stream-cdr strms)))
		(else (stream-cons
			 (stream-car (stream-car strms))
			 (stream-concat
			    (stream-cons (stream-cdr (stream-car strms)) (stream-cdr strms))))))))
    (if (not (stream? strms))
        (error 'stream-concat "non-stream argument" strms)
        (stream-concat strms)))

(define stream-constant
   (stream-lambda objs
      (cond ((null? objs) stream-null)
            ((null? (cdr objs)) (stream-cons (car objs) (stream-constant (car objs))))
            (else (stream-cons (car objs)
		     (apply stream-constant (append (cdr objs) (list (car objs)))))))))

(define (stream-drop n strm)
   (define stream-drop
      (stream-lambda (n strm)
	 (if (or (zero? n) (stream-null? strm))
	     strm
	     (stream-drop (- n 1) (stream-cdr strm)))))
   (cond ((not (integer? n)) (error 'stream-drop "non-integer argument" n))
          ((negative? n) (error 'stream-drop "negative argument" n))
          ((not (stream? strm)) (error 'stream-drop "non-stream argument" strm))
          (else (stream-drop n strm))))

(define (stream-drop-while pred? strm)
   (define stream-drop-while
       (stream-lambda (strm)
	  (if (and (stream-pair? strm) (pred? (stream-car strm)))
	      (stream-drop-while (stream-cdr strm))
	      strm)))
   (cond ((not (procedure? pred?)) (error 'stream-drop-while "non-procedural argument" strm))
	 ((not (stream? strm)) (error 'stream-drop-while "non-stream argument" strm))
	 (else (stream-drop-while strm))))

(define (stream-filter pred? strm)
   (define stream-filter
      (stream-lambda (strm)
	 (cond ((stream-null? strm) stream-null)
	       ((pred? (stream-car strm))
                (stream-cons (stream-car strm) (stream-filter (stream-cdr strm))))
	       (else (stream-filter (stream-cdr strm))))))
   (cond ((not (procedure? pred?)) (error 'stream-filter "non-procedural argument" pred?))
	 ((not (stream? strm)) (error 'stream-filter "non-stream argument" strm))
          (else (stream-filter strm))))

(define (stream-fold proc base strm)
   (cond ((not (procedure? proc)) (error 'stream-fold "non-procedural argument" proc))
	 ((not (stream? strm)) (error 'stream-fold "non-stream argument" strm))
	 (else (let loop ((base base) (strm strm))
                  (if (stream-null? strm)
                      base
                      (loop (proc base (stream-car strm)) (stream-cdr strm)))))))

(define (stream-for-each proc . strms)
   (define (stream-for-each strms)
      (if (not (exists stream-null? strms))
          (begin (apply proc (map stream-car strms))
                 (stream-for-each (map stream-cdr strms)))))
   (cond ((not (procedure? proc)) (error 'stream-for-each "non-procedural argument" proc))
          ((null? strms) (error 'stream-for-each "no stream arguments" strms))
          ((exists (lambda (x) (not (stream? x))) strms)
	   (error 'stream-for-each "non-stream argument" strms))
          (else (stream-for-each strms))))

(define (stream-from first . step)
   (define stream-from
      (stream-lambda (first delta)
	 (stream-cons first (stream-from (+ first delta) delta))))
   (let ((delta (if (null? step) 1 (car step))))
      (cond ((not (number? first)) (error 'stream-from "non-numeric starting number" first))
            ((not (number? delta)) (error 'stream-from "non-numeric step size" delta))
            (else (stream-from first delta)))))

(define (stream-iterate proc base)
   (define stream-iterate
      (stream-lambda (base)
	 (stream-cons base (stream-iterate (proc base)))))
   (if (not (procedure? proc))
       (error 'stream-iterate "non-procedural argument" proc)
       (stream-iterate base)))

(define (stream-length strm)
    (if (not (stream? strm))
        (error 'stream-length "non-stream argument" strm)
        (let loop ((len 0) (strm strm))
	   (if (stream-null? strm)
	       len
	       (loop (+ len 1) (stream-cdr strm))))))



(define (stream-map proc . strms)
   (define stream-map
      (stream-lambda (strms)
	 (if (exists stream-null? strms)
	     stream-null
	     (stream-cons (apply proc (map stream-car strms))
		(stream-map (map stream-cdr strms))))))
   (cond ((not (procedure? proc)) (error 'stream-map "non-procedural argument" proc))
	 ((null? strms) (error 'stream-map "no stream arguments" strms))
	 ((exists (lambda (x) (not (stream? x))) strms)
	  (error 'stream-map "non-stream argument" strms))
	 (else (stream-map strms))))


(define (stream-range first past . step)
   (define stream-range
      (stream-lambda (first past delta lt?)
	 (if (lt? first past)
	     (stream-cons first (stream-range (+ first delta) past delta lt?))
	     stream-null)))
   (cond ((not (number? first)) (error 'stream-range "non-numeric starting number" first))
	 ((not (number? past)) (error 'stream-range "non-numeric ending number"past ))
	 (else (let ((delta (cond ((pair? step) (car step)) ((< first past) 1) (else -1))))
                  (if (not (number? delta))
                      (error 'stream-range "non-numeric step size" delta)
                      (let ((lt? (if (< 0 delta) < >)))
			 (stream-range first past delta lt?)))))))

(define (stream-ref strm n)
   (cond ((not (stream? strm)) (error 'stream-ref "non-stream argument" strm))
	 ((not (integer? n)) (error 'stream-ref "non-integer argument" n))
	 ((negative? n) (error 'stream-ref "negative argument" n))
	 (else (let loop ((strm strm) (n n))
                  (cond ((stream-null? strm) (error 'stream-ref "beyond end of stream" strm))
                        ((zero? n) (stream-car strm))
                        (else (loop (stream-cdr strm) (- n 1))))))))

(define (stream-reverse strm)
   (define stream-reverse
      (stream-lambda (strm rev)
	 (if (stream-null? strm)
	     rev
	     (stream-reverse (stream-cdr strm) (stream-cons (stream-car strm) rev)))))
   (if (not (stream? strm))
       (error 'stream-reverse "non-stream argument" strm)
       (stream-reverse strm stream-null)))

(define (stream-scan proc base strm)
   (define stream-scan
      (stream-lambda (base strm)
	 (if (stream-null? strm)
	     (stream base)
	     (stream-cons base (stream-scan (proc base (stream-car strm)) (stream-cdr strm))))))
   (cond ((not (procedure? proc)) (error 'stream-scan "non-procedural argument" proc))
	 ((not (stream? strm)) (error 'stream-scan "non-stream argument" strm))
	 (else (stream-scan base strm))))

(define (stream-take n strm)
   (define stream-take
      (stream-lambda (n strm)
	 (if (or (stream-null? strm) (zero? n))
	     stream-null
	     (stream-cons (stream-car strm) (stream-take (- n 1) (stream-cdr strm))))))
   (cond ((not (stream? strm)) (error 'stream-take "non-stream argument" strm))
	 ((not (integer? n)) (error 'stream-take "non-integer argument" n))
	 ((negative? n) (error 'stream-take "negative argument" n))
          (else (stream-take n strm))))

(define (stream-take-while pred? strm)
   (define stream-take-while
      (stream-lambda (strm)
	 (cond ((stream-null? strm) stream-null)
	       ((pred? (stream-car strm))
                (stream-cons (stream-car strm) (stream-take-while (stream-cdr strm))))
	       (else stream-null))))
   (cond ((not (stream? strm)) (error 'stream-take-while "non-stream argument" strm))
	 ((not (procedure? pred?)) (error 'stream-take-while "non-procedural argument" pred?))
	 (else (stream-take-while strm))))

(define (stream-unfold mapper pred? generator base)
   (define stream-unfold
      (stream-lambda (base)
	 (if (pred? base)
	     (stream-cons (mapper base) (stream-unfold (generator base)))
	     stream-null)))
   (cond ((not (procedure? mapper)) (error 'stream-unfold "non-procedural mapper" mapper))
	 ((not (procedure? pred?)) (error 'stream-unfold "non-procedural pred?" pred?))
	 ((not (procedure? generator)) (error 'stream-unfold "non-procedural generator" generator))
	 (else (stream-unfold base))))

(define (stream-unfolds gen seed)
   (define (len-values gen seed)
      (call-with-values
	 (lambda () (gen seed))
	 (lambda vs (- (length vs) 1))))
   (define unfold-result-stream
      (stream-lambda (gen seed)
	 (call-with-values
	    (lambda () (gen seed))
	    (lambda (next . results)
	       (stream-cons results (unfold-result-stream gen next))))))
   (define result-stream->output-stream
      (stream-lambda (result-stream i)
	 (let ((result (list-ref (stream-car result-stream) (- i 1))))
	    (cond ((pair? result)
		   (stream-cons
		      (car result)
		      (result-stream->output-stream (stream-cdr result-stream) i)))
		  ((not result)
		   (result-stream->output-stream (stream-cdr result-stream) i))
                ((null? result) stream-null)
                (else (error 'stream-unfolds "can't happen" #unspecified))))))
   (define (result-stream->output-streams result-stream)
      (let loop ((i (len-values gen seed)) (outputs '()))
	 (if (zero? i)
	     (apply values outputs)
	     (loop (- i 1) (cons (result-stream->output-stream result-stream i) outputs)))))
   (if (not (procedure? gen))
       (error 'stream-unfolds "non-procedural argument" gen)
       (result-stream->output-streams (unfold-result-stream gen seed))))

(define (stream-zip . strms)
   (define stream-zip
      (stream-lambda (strms)
	 (if (exists stream-null? strms)
	     stream-null
	     (stream-cons (map stream-car strms) (stream-zip (map stream-cdr strms))))))
   (cond ((null? strms) (error 'stream-zip "no stream arguments" strms))
	 ((exists (lambda (x) (not (stream? x))) strms)
	  (error 'stream-zip "non-stream argument" strms))
	 (else (stream-zip strms))))


(define-stream (stream-merge lt? . strms)
   (define-stream (merge xx yy)
      (stream-match xx (() yy) ((x . xs)
				(stream-match yy (() xx) ((y . ys)
        (if (lt? y x)
            (stream-cons y (merge xx ys))
            (stream-cons x (merge xs yy))))))))
   (stream-let loop ((strms strms))
      (cond ((null? strms) stream-null)
	    ((null? (cdr strms)) (car strms))
	    (else (merge (car strms)
		     (apply stream-merge lt?
			(cdr strms)))))))

(define-stream (stream-unique eql? strm)
  (if (stream-null? strm)
      stream-null
      (stream-cons (stream-car strm)
        (stream-unique eql?
          (stream-drop-while
            (lambda (x)
              (eql? (stream-car strm) x))
            strm)))))

