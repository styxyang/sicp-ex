;; fast-prime? copied from previous exercises
(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (define (square base)
    (* base base))
  (define (even? exp)
    (= (remainder exp 2) 0))
  (cond ((= 0 exp) 1)
	((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
	(else (remainder (* base (expmod base (- exp 1) m)) m))))

;; filtered-accumulate
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter x result)
    (if (> x b)
	result
	(iter (next x)
	      ;; `filter' is designed to accept two parameters
	      ;; in order to apply filter to both
	      ;; pre and post application of `term'
	      (combiner (filter x (term x)) result))))
  (iter a null-value))

;; sum-prime
(define (sum filter term a next b)
  (filtered-accumulate filter + 0 term a next b))

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

(define (prime-filter x result)
  (if (fast-prime? x 10)
      result
      0))

;; 2**2 + 3**2 + 5**2 + 7**2
(sum prime-filter square 2 inc 8) ;; I didn't handle `1'


;; n-prime-product
(define (product filter term a next b)
  (filtered-accumulate filter * 1 term a next b))

(define (identity x)
  x)

(define (n-prime-filter x result)
  (define (gcd m n)
    ;; assume `m' < `n'
    (if (= 0 (remainder n m))
	m
	(gcd (remainder n m) m)))
  ;; I think the filter should be provided by user
  ;; of this function, so the `n' should come from
  ;; the user as wel, :)
  (if (= 1 (gcd x 9))
      result
      1))

;; 1 * 2 * 4 * 5 * 7 * 8
(product n-prime-filter identity 1 inc 9)
