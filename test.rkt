#lang dan-scheme

(require rackunit)

(define (f x arg)
  (+ (add1 x) 3))

(check-equal? (f 3 'whatever) 7)

(define foo
  (lambda args (cons 1 args)))

(check-equal? (map (λ (x) (add1 x)) '(1 2 3 4))
              '(2 3 4 5))

(check-equal? (map foo '(a b c))
              '((1 a) (1 b) (1 c)))
