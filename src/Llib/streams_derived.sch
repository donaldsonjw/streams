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

(define-syntax define-stream
    (syntax-rules ()
      ((define-stream (name . formal) body0 body1 ...)
        (define name (stream-lambda formal body0 body1 ...)))))

(define-syntax stream
    (syntax-rules ()
      ((stream) stream-null)
      ((stream x y ...) (stream-cons x (stream y ...)))))

(define-syntax stream-let
    (syntax-rules ()
      ((stream-let tag ((name val) ...) body1 body2 ...)
       ((letrec ((tag (stream-lambda (name ...) body1 body2 ...))) tag) val ...))))

  (define-syntax stream-match
    (syntax-rules ()
      ((stream-match strm-expr clause ...)
        (let ((strm strm-expr))
          (cond
            ((not (stream? strm)) (error 'stream-match "non-stream argument" strm))
            ((stream-match-test strm clause) => car) ...
            (else (error 'stream-match "pattern failure" #unspecified)))))))

  (define-syntax stream-match-test
    (syntax-rules ()
      ((stream-match-test strm (pattern fender expr))
        (stream-match-pattern strm pattern () (and fender (list expr))))
      ((stream-match-test strm (pattern expr))
        (stream-match-pattern strm pattern () (list expr)))))

   (define-expander stream-match-pattern
      (lambda (x e)
	 (define (wildcard? x)
	    (and (symbol? x)
		 (eq? x '_)))
	 (match-case x
	    ((?- ?strm () (and ?binding (??-)) ?body)
	     (e `(and (stream-null? ,strm) (let ,binding ,body)) e))
	    ((?- ?strm ((and ?w (? wildcard?)) . ?rest)
		(and ?binding (??-)) ?body)
	     (define strm2 (gensym 'strm2))
	     (e `(and (stream-pair? ,strm)
		      (let ((,strm2 (stream-cdr ,strm)))
			 (stream-match-pattern ,strm2 ,rest ,binding ,body))) e))
	    ((?- ?strm (?var . ?rest) (and ?binding (??-)) ?body)
	     (define temp (gensym 'temp))
	     (define strm2 (gensym 'strm2))
	     (e `(and (stream-pair? ,strm)
                        (let ((,temp (stream-car ,strm))
			      (,strm2 (stream-cdr ,strm))) 
                          (stream-match-pattern ,strm2 ,rest
			     ((,var ,temp) . ,binding) ,body))) e))
	    ((?- ?strm (and ?w (? wildcard?)) (and ?binding (??-)) ?body)
	     (e `(let ,binding ,body) e))
	    ((?- ?strm ?var (and ?binding (??-)) ?body)
	     (e `(let ((,var ,strm) . ,binding) ,body) e))
	    (else (error "stream-match-pattern" "illegal expression" x)))))

  (define-syntax stream-of
    (syntax-rules ()
      ((_ expr rest ...)
        (stream-of-aux expr stream-null rest ...))))

  (define-syntax stream-of-aux
    (syntax-rules (in is)
      ((stream-of-aux expr base)
        (stream-cons expr base))
      ((stream-of-aux expr base (var in stream) rest ...)
        (stream-let loop ((strm stream))
          (if (stream-null? strm)
              base
              (let ((var (stream-car strm)))
                (stream-of-aux expr (loop (stream-cdr strm)) rest ...)))))
      ((stream-of-aux expr base (var is exp) rest ...)
        (let ((var exp)) (stream-of-aux expr base rest ...)))
      ((stream-of-aux expr base pred? rest ...)
        (if pred? (stream-of-aux expr base rest ...) base))))
