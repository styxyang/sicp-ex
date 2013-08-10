;; definition of recursive accumulate
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

;; reimplement sum and product
(define (sum term a next b)
  (define (add x y)
    (+ x y))
  ;; here I replace `add' with `+' and it seems ok
  (accumulate + 0 term a next b))

(define (product term a next b)
  (define (mul x y)
    (* x y))
  ;; so again I replace `mul' with `*'
  (accumulate * 1 term a next b))

;; test
(define (cube x)
  (* x x x))

(define (inc x)
  (+ x 1))

(sum cube 1 inc 3)
(product cube 1 inc 3)
