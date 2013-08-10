;; definition of product product-iter
(define (product-iter term a next b)
  (define (iter x result)
    (if (> x b)
	result
	(iter (next x) (* (term x) result))))
  (iter a 1))

;; jw-pi with product-iter
(define (jw-pi n)
  (define (fraction x)
    (* 1.0 (/ (* 2 (+ (quotient x 2) 1))
	      (+ 1 (* 2 (quotient (+ x 1) 2))))))
  (define (inc x)
    (+ x 1))
  (* 4 (product-iter fraction 1 inc n)))

;; test
(jw-pi 1000)
