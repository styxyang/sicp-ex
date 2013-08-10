;; definition of product
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

;; John Wallis approximation
(define (jw-pi n)
  (define (fraction x)
    (* 1.0 (/ (* 2 (+ (quotient x 2) 1))
	      (+ 1 (* 2 (quotient (+ x 1) 2))))))
  (define (inc x)
    (+ x 1))
  (* 4 (product fraction 1 inc n)))

;; test
(jw-pi 1000)

(provide jw-pi)
