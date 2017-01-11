#lang dan-scheme

(require rackunit)

(define (f x arg)
  (+ (add1 x) 3))

(define foo
  (lambda args (cons 1 args)))

