#lang racket

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter x result)
    (if (> x a)
	result
	(iter (next x) (* (term x) result))))
  (iter a 1))

(provide (combine-out product product-iter))
