;; The sum of the squares of the first ten natural numbers is,

;; 12 + 22 + ... + 102 = 385
;; The square of the sum of the first ten natural numbers is,

;; (1 + 2 + ... + 10)2 = 552 = 3025
;; Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

;; Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
;; ---------


(define (get-diff-sum_sq-sq_sum-first-hundred)
  (define first-hundred (enumerate-interval 1 100))  
  (- (sum-sq first-hundred)
     (sq-sum first-hundred)))

(define (sum-sq sequence)
  (define sq-sequence
    (map square sequence))
  (accumulate + 0 sq-sequence))

(define (sq-sum sequence)
  (square (accumulate + 0 sequence)))



(define (square x)
  (* x x))

;; ---------[list interface] ---------


(define (make-number-pairs low high)
  (flatmap-gw
   (lambda (i)
     (map (lambda (j) (list i j))
	  (enumerate-interval low i)))
   (enumerate-interval low high)))

(define (pair-to-product pair)
  (* (car pair) (cadr pair)))

(define (max-in-list sequence)
  (max-in-list-iter sequence 0))

(define (max-in-list-iter sequence max)
  (if (null? sequence)
      max
      (if (> (car sequence) max)
	  (max-in-list-iter (cdr sequence) (car sequence))
	  (max-in-list-iter (cdr sequence) max))))

;; done! gw:need to review nested loop and mapping functions
;; done! gw: see test7.scm


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap-gw proc seq)
  (accumulate append '() (map proc seq)))

(define (make-nested-loop-pairs-gw n)
  (flatmap-gw
   (lambda (i)
     (map (lambda (j) (list i j))
	  (enumerate-interval 1 i)))
   (enumerate-interval 1 n)))


(define (filter predicate sequence)
  (if (null? sequence)
      '()
      (if (predicate (car sequence))
	  (cons (car sequence) (filter predicate (cdr sequence)))
	  (filter predicate (cdr sequence)))))

;; ---------[/list interface] ---------

;; ---------[string ADT]---------
;; test ok
(define (make-empty-string)
  '())

;; test ok
(define (string-push-front string char)
  (cons char string))

;; test ok
(define (string-push-back string char)
  (append string
	  (cons char (make-empty-string))))

;; test ok
(define (string-length string)
  (if (null? string)
      0
      (+ 1 (string-length (cdr string)))))

;; test ok
(define (string-at string n)
  (if (null? string)
      '()
      (if (= n 0)
	  (car string)
	  (string-at (cdr string) (- n 1)))))


;; test ok
(define (string-pop-back s)
  (if (null? s)
      '()
      (if (null? (cdr s))
	  (car s)
	  (string-pop-back (cdr s)))))
;; test ok
(define (string-pop-front s)
  (if (null? s)
      '()
      (car s)))

;; test ok
(define (string-all-but-last s)
  (if (null? s)
      '()
      (if (null? (cdr s))
	  '()
	  (cons (car s) (string-all-but-last (cdr s))))))

;; test ok
(define (string-middle s)
  (if (< (string-length s) 3)
      '()
      (cdr (string-all-but-last s))))

;; test ok
(define (integer-to-string n)
  (if (< n 10)
      (cons (digit-to-char n) (make-empty-string))
      (string-push-back (integer-to-string (quotient n 10)) (digit-to-char (modulo n 10)))))



(define (digit-to-char d)
  (cond
   ((= d 0)  '0)
   ((= d 1)  '1)
   ((= d 2)  '2)
   ((= d 3)  '3)
   ((= d 4)  '4)
   ((= d 5)  '5)
   ((= d 6)  '6)
   ((= d 7)  '7)
   ((= d 8)  '8)
   ((= d 9)  '9)
   (else  'err)))
;; ---------[/string ADT]---------
