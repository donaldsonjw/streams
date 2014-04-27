(module teststreams
   (library streams)
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




(define (main args)

   (print (stream->list (stream-take 3 hamming)))


   (stream-match (list->stream '(1 2 3 4 5))
      (() (print "empty"))
      ((one two three . rest)
       (print one two three)
       (print (stream->list (stream-take 2 rest)))
       ))


   (print (stream->list
	     (stream-concat
		(stream
		   (stream 1 2) (stream) (stream 3 2 1)))))

   ;(print (stream->list (stream-take 3 (stream (stream 1 2 3 4 5 6)))))
   
   #;(print (stream->list (perms (stream 1 2 3 4 5)))))




