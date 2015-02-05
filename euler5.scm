

;; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
(declare (usual-integrations))
(define (get-smallest-pos-divisible-int low high)
  (define (divisible-seq low high)
    (define (range-divisible? low high n)
      (if (= low high)
	  true
	  (if (= (modulo n low) 0)
	      (range-divisible? (+ low 1) high n)
	      false)))			;assume low < high, TODO later: read abt exception
    ;; (stream-filter (range-divisible? low high) integers)
        (stream-filter (lambda (x) (range-divisible? low high x)) integers)
    )			;done!TODO: load stream
  (stream-ref (divisible-seq low high) 0))


;; ---------[stream interface]---------
   
   
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))
						      
						      
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))


	      


;; ---------[/stream interface]---------
