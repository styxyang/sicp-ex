;; iterative definition
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))

;; code from 1.29 for test
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (simpson f a b n)
  (define (h)
    (/ (* (- b a) 1.0) n))
  (define (inc x)
    (+ x 1))
  (define (even? x)
    (= (remainder x 2) 0))
  (define (y k)
    (f (+ a (* k (h)))))
  (define (fp k)
    (* (y k) (cond ((= k 0) 1)
		   ((= k n) 1)
		   ((even? k) 2)
		   (else 4))))
  ;; replace sum for sum-iter to test the result
  (/ (* (h) (sum fp 0 inc n)) 3))

(define (cube x)
  (* x x x))

;; test
(simpson cube 0 1 10000000)

