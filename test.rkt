#lang dan-scheme

(define (f x  args)
  (+ (add1 x) 3))

(define foo
  (lambda args (cons 1 args)))