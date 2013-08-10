;; definition of recursive accumulate
(define (accumulate-iter combiner null-value term a next b)
  (define (iter x result)
    (if (> x b)
	result
	(iter (next x)
	      (combiner (term x) result))))
  (iter a null-value))

;; reimplement sum and product
(define (sum term a next b)
  (define (add x y)
    (+ x y))
  ;; here I replace `add' with `+' and it seems ok
  (accumulate-iter + 0 term a next b))

(define (product term a next b)
  (define (mul x y)
    (* x y))
  ;; so again I replace `mul' with `*'
  (accumulate-iter * 1 term a next b))

;; test
(define (cube x)
  (* x x x))

(define (inc x)
  (+ x 1))

(sum cube 1 inc 3)
(product cube 1 inc 3)
