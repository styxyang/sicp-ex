;; template for sum
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

;; wrap f in fp with classified discussion
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
  (/ (* (h) (sum fp 0 inc n)) 3))

;; test
(define (cube x)
  (* x x x))

(simpson cube 0 1 100)
