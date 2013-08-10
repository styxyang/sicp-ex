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
  (define (fp x)
    (cond ((= x 0) (f (+ a (* x (h)))))
	  ((= x n) (f (+ a (* x (h)))))
	  ((even? x) (* 2 (f (+ a (* x (h))))))
	  (else (* 4 (f (+ a (* x (h))))))))
  (/ (* (h) (sum fp 0 inc n)) 3))

;; test
(define (cube x)
  (* x x x))

(simpson cube 0 1 100)
